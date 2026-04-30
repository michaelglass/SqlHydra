/// Normalizes LINQ expression trees before visitor processing.
/// Newer FSharp.Core versions (10.1+) emit different expression tree shapes:
/// - Lambda bodies wrapped in BlockExpressions with local variables
/// - Comparison operators as MethodCall expressions instead of BinaryExpression nodes
/// This module transforms those into predictable shapes so visitors don't need per-version handling.
module internal SqlHydra.Query.ExpressionNormalizer

open System.Linq.Expressions

/// Substitutes ParameterExpression references with their assigned values.
type private VariableInliner(substitutions: System.Collections.Generic.Dictionary<ParameterExpression, Expression>) =
    inherit ExpressionVisitor()
    override this.VisitParameter(node) =
        match substitutions.TryGetValue(node) with
        | true, value -> this.Visit(value)
        | _ -> node :> Expression

/// Maps comparison operator method names to their corresponding ExpressionType.
let private tryGetComparisonExpressionType (methodName: string) =
    match methodName with
    | "op_Equality" | "GenericEqualityIntrinsic" -> Some ExpressionType.Equal
    | "op_Inequality" | "GenericInequalityIntrinsic" -> Some ExpressionType.NotEqual
    | "op_GreaterThan" | "GenericGreaterThanIntrinsic" -> Some ExpressionType.GreaterThan
    | "op_GreaterThanOrEqual" | "GenericGreaterOrEqualIntrinsic" -> Some ExpressionType.GreaterThanOrEqual
    | "op_LessThan" | "GenericLessThanIntrinsic" -> Some ExpressionType.LessThan
    | "op_LessThanOrEqual" | "GenericLessOrEqualIntrinsic" -> Some ExpressionType.LessThanOrEqual
    | _ -> None

/// Recursively normalizes expression trees:
/// 1. Inlines BlockExpression variables (preserving tuple deconstructions)
/// 2. Converts comparison operator MethodCalls to BinaryExpression nodes
type private Normalizer() =
    inherit ExpressionVisitor()

    override this.VisitBlock(node) =
        // Build substitution map for non-tuple-deconstruction variables.
        // Variables assigned from MemberAccess (e.g., o = tupledArg.Item1) are preserved
        // because visitAlias extracts table aliases from ParameterExpression names.
        let substitutions = System.Collections.Generic.Dictionary<ParameterExpression, Expression>()
        for expr in node.Expressions do
            if expr.NodeType = ExpressionType.Assign then
                let bin = expr :?> BinaryExpression
                match bin.Left with
                | :? ParameterExpression as p when bin.Right.NodeType <> ExpressionType.MemberAccess ->
                    substitutions.[p] <- bin.Right
                | _ -> ()
        let result = node.Expressions |> Seq.last
        let inlined =
            if substitutions.Count > 0 then
                VariableInliner(substitutions).Visit(result)
            else
                result
        // Continue normalizing the inlined result (handles nested blocks, operator calls, etc.)
        this.Visit(inlined)

    override this.VisitMethodCall(node) =
        match tryGetComparisonExpressionType node.Method.Name with
        | Some exprType when node.Arguments.Count = 2 ->
            let left = this.Visit(node.Arguments.[0])
            let right = this.Visit(node.Arguments.[1])
            Expression.MakeBinary(exprType, left, right) :> Expression
        | _ ->
            base.VisitMethodCall(node)

/// Normalizes a LINQ expression tree into a predictable shape for visitor processing.
let normalize (expr: Expression) : Expression =
    Normalizer().Visit(expr)

// ─── NormalizedExpression AST ───────────────────────────────────────────────────
// A stable, compiler-agnostic representation of expression trees.
// Structural noise (Lambda, Block, Invoke) is eliminated during conversion.
// Original LINQ types are retained in variants for semantic pattern matching.

[<NoComparison>]
type NormalizedExpression =
    | NBinary of left: NormalizedExpression * op: ExpressionType * right: NormalizedExpression
    | NUnary of op: ExpressionType * operand: NormalizedExpression
    | NMethodCall of call: MethodCallExpression * args: NormalizedExpression list
    | NMemberAccess of inner: NormalizedExpression * memberExpr: MemberExpression
    | NConstant of value: obj * exprType: System.Type
    | NParameter of ParameterExpression
    | NNew of newExpr: NewExpression * args: NormalizedExpression list
    | NConditional of test: NormalizedExpression * ifTrue: NormalizedExpression * ifFalse: NormalizedExpression
    | NUnknown of Expression

/// Converts a normalized LINQ expression tree into a NormalizedExpression AST.
/// Handles Lambda unwrapping, Block unwrapping, and Invoke unwrapping centrally
/// so that downstream visitors only see stable, predictable shapes.
let rec visitExpression (exp: Expression) : NormalizedExpression =
    match exp with
    // Structural noise — unwrap and recurse
    | :? LambdaExpression as lam ->
        visitExpression lam.Body
    | :? BlockExpression as blk ->
        visitExpression (blk.Expressions |> Seq.last)
    | :? MethodCallExpression as m when m.Method.Name = "Invoke" && m.Object <> null ->
        // Unwrap Invoke on Lambda — this is the F# CE tuple parameter pattern.
        // Do NOT unwrap Invoke on FSharpFunc closures (those are real function calls).
        match m.Object with
        | :? LambdaExpression -> visitExpression m.Object
        | _ -> NMethodCall(m, m.Arguments |> Seq.map visitExpression |> Seq.toList)
    // Semantic nodes — convert to AST
    | :? UnaryExpression as u ->
        NUnary(u.NodeType, visitExpression u.Operand)
    | :? BinaryExpression as b ->
        NBinary(visitExpression b.Left, b.NodeType, visitExpression b.Right)
    | :? MethodCallExpression as m ->
        NMethodCall(m, m.Arguments |> Seq.map visitExpression |> Seq.toList)
    | :? MemberExpression as m ->
        let inner =
            if m.Expression <> null then visitExpression m.Expression
            else NConstant(null, typeof<obj>)
        NMemberAccess(inner, m)
    | :? ConstantExpression as c ->
        NConstant(c.Value, c.Type)
    | :? ParameterExpression as p ->
        NParameter(p)
    | :? NewExpression as n ->
        NNew(n, n.Arguments |> Seq.map visitExpression |> Seq.toList)
    | :? ConditionalExpression as c ->
        NConditional(visitExpression c.Test, visitExpression c.IfTrue, visitExpression c.IfFalse)
    | _ ->
        NUnknown(exp)

/// Normalizes a LINQ expression tree and converts it to a NormalizedExpression AST.
/// This is the primary entry point: normalize (Phase 1) then convert to AST (Phase 2).
let toNormalizedExpression (expr: Expression) : NormalizedExpression =
    visitExpression (normalize expr)
