module internal SqlHydra.Query.LinqExpressionVisitors

open System
open System.Linq.Expressions
open System.Reflection
open SqlKata
open FastExpressionCompiler

let notImpl() = raise (NotImplementedException())
let notImplMsg msg = raise (NotImplementedException msg)


[<AutoOpen>]
module VisitorPatterns =

    let (|Lambda|_|) (exp: Expression) =
        match exp.NodeType with
        | ExpressionType.Lambda -> Some (exp :?> LambdaExpression)
        | _ -> None

    let (|Unary|_|) (exp: Expression) =
        match exp.NodeType with
        | ExpressionType.ArrayLength
        | ExpressionType.Convert
        | ExpressionType.ConvertChecked
        | ExpressionType.Negate
        | ExpressionType.UnaryPlus
        | ExpressionType.NegateChecked
        | ExpressionType.Not
        | ExpressionType.Quote
        | ExpressionType.TypeAs -> Some (exp :?> UnaryExpression)
        | _ -> None

    let (|Binary|_|) (exp: Expression) =
        match exp.NodeType with
        | ExpressionType.Add
        | ExpressionType.AddChecked
        | ExpressionType.And
        | ExpressionType.AndAlso
        | ExpressionType.ArrayIndex
        | ExpressionType.Coalesce
        | ExpressionType.Divide
        | ExpressionType.Equal
        | ExpressionType.ExclusiveOr
        | ExpressionType.GreaterThan
        | ExpressionType.GreaterThanOrEqual
        | ExpressionType.LeftShift
        | ExpressionType.LessThan
        | ExpressionType.LessThanOrEqual
        | ExpressionType.Modulo
        | ExpressionType.Multiply
        | ExpressionType.MultiplyChecked
        | ExpressionType.NotEqual
        | ExpressionType.Or
        | ExpressionType.OrElse
        | ExpressionType.Power
        | ExpressionType.RightShift
        | ExpressionType.Subtract
        | ExpressionType.SubtractChecked -> Some (exp :?> BinaryExpression)
        | _ -> None

    let (|MethodCall|_|) (exp: Expression) =
        match exp.NodeType with
        | ExpressionType.Call -> Some (exp :?> MethodCallExpression)    
        | _ -> None
    let (|New|_|) (exp: Expression) =
        match exp.NodeType with
        | ExpressionType.New -> Some (exp :?> NewExpression)
        | _ -> None

    let (|Constant|_|) (exp: Expression) =
        match exp.NodeType with
        | ExpressionType.Constant -> Some (exp :?> ConstantExpression)
        | _ -> None
    
    let (|ImplConvertConstant|_|) (exp: Expression) =
        match exp.NodeType with
        | ExpressionType.Convert ->
            // Handles implicit conversion. Ex: upcasting int to an int64
            let unary = exp :?> UnaryExpression
            match unary.Operand with
            | Constant c when unary.Type.IsPrimitive -> Some c
            | _ -> None
            //Some (unary.Operand, unary.Type)
        | ExpressionType.Call -> 
            // Handles implicit conversion. Ex: casting an int to a decimal
            let mc = exp :?> MethodCallExpression
            match mc.Method.Name, mc.Arguments |> Seq.toList with
            | "op_Implicit", [ Constant c ] -> Some c
            | _ -> None
        | _ -> None
    
    let (|ArrayInit|_|) (exp: Expression) =
        match exp.NodeType with
        | ExpressionType.NewArrayInit -> 
            let arrayExp = exp :?> NewArrayExpression
            Some (arrayExp.Expressions |> Seq.map (function | Constant c -> c.Value | _ -> notImplMsg "Unable to unwrap array value."))
        | _ -> None

    let rec unwrapListExpr (lstValues: obj list, lstExp: MethodCallExpression) =
        if lstExp.Arguments.Count > 0 then
            match lstExp.Arguments.[0] with
            | Constant c -> unwrapListExpr (lstValues @ [c.Value], (lstExp.Arguments.[1] :?> MethodCallExpression))
            | _ -> notImpl()
        else 
            lstValues    

    let (|ListInit|_|) (exp: Expression) = 
        match exp with
        | MethodCall c when c.Method.Name = "Cons" ->
            let values = unwrapListExpr ([], c)
            Some values
        | _ -> None

    let (|Member|_|) (exp: Expression) =
        match exp.NodeType with
        | ExpressionType.MemberAccess -> Some (exp :?> MemberExpression)
        | _ -> None

    let (|BoolMember|_|) (exp: Expression) = 
        match exp with
        | Member m when m.Type = typeof<bool> -> Some m
        | _ -> None

    let (|BoolConstant|_|) (exp: Expression) = 
        match exp with
        | Constant c when c.Type = typeof<bool> -> Some (c.Value :?> bool)
        | _ -> None

    let (|Parameter|_|) (exp: Expression) =
        match exp.NodeType with
        | ExpressionType.Parameter -> Some (exp :?> ParameterExpression)
        | _ -> None

[<AutoOpen>]
module SqlPatterns = 

    let (|Not|_|) (exp: Expression) = 
        match exp.NodeType with
        | ExpressionType.Not -> Some ((exp :?> UnaryExpression).Operand)
        | _ -> None

    let (|BinaryAnd|_|) (exp: Expression) =
        match exp.NodeType with
        | ExpressionType.And
        | ExpressionType.AndAlso -> Some (exp :?> BinaryExpression)
        | _ -> None

    let (|BinaryOr|_|) (exp: Expression) =
        match exp.NodeType with
        | ExpressionType.Or
        | ExpressionType.OrElse -> Some (exp :?> BinaryExpression)
        | _ -> None

    let (|BinaryCompare|_|) (exp: Expression) =
        match exp.NodeType with
        | ExpressionType.Equal
        | ExpressionType.NotEqual
        | ExpressionType.GreaterThan
        | ExpressionType.GreaterThanOrEqual
        | ExpressionType.LessThan
        | ExpressionType.LessThanOrEqual -> Some (exp :?> BinaryExpression)
        | _ -> None

    let (|Call|_|) (exp: Expression) =
        match exp.NodeType with
        | ExpressionType.Call -> Some (exp :?> MethodCallExpression)
        | _ -> None

    let isOptionType (t: Type) = 
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Option<_>>

    let isNullableType (t: Type) = 
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Nullable<_>>

    let isOptionOrNullableType (t: Type) = 
        t.IsGenericType && (
            let genericTypeDef = t.GetGenericTypeDefinition()
            genericTypeDef = typedefof<Option<_>> || 
            genericTypeDef = typedefof<Nullable<_>>
        )

    let tryGetMember(x: Expression) = 
        match x with
        | Member m when m.Expression = null -> 
            None
        | Member m when m.Expression.NodeType = ExpressionType.Parameter || m.Expression.NodeType = ExpressionType.MemberAccess -> 
            Some m
        | MethodCall opt when opt.Type |> isOptionType ->        
            if opt.Arguments.Count > 0 then
                // Option.Some
                match opt.Arguments.[0] with
                | Member m -> Some m
                | _ -> None
            else None
        | MethodCall nul when nul.Type |> isNullableType -> 
            if nul.Arguments.Count > 0 then
                // Nullable.Value
                match nul.Arguments.[0] with
                | Member m -> Some m
                | _ -> None
            else None
        | Unary u when u.Operand.NodeType = ExpressionType.MemberAccess -> 
            Some (u.Operand :?> MemberExpression)
        | _ -> 
            None
                
    // Extract constant value from nested object/properties
    let rec unwrapMember (m: MemberExpression) =
        match m.Expression with
        | Constant c -> Some c.Value
        | Member m -> unwrapMember m
        | _ -> None

    let compileAndEvaluateExpression (exp: Expression) = 
        try
            let lambda = Expression.Lambda(exp)
            let compiled = lambda.CompileFast()
            compiled.DynamicInvoke()
        with ex ->  
            notImplMsg $"Unable to evaluate query parameter expression:\n{exp}"

    /// Handles extended properties on Nullable and Option types.
    [<RequireQualifiedAccess>]
    type ExtProperty = 
        | IsSome
        | IsNone
        | HasValue
        | Value
        | NA

    /// A property member with extended property info for Nullable and Option types.
    let (|Property|_|) (exp: Expression) =
        match exp with
        | Member m when 
            m.Member.DeclaringType <> null && 
            m.Member.DeclaringType |> isOptionOrNullableType && 
            (m.Member.Name = "Value" || m.Member.Name = "HasValue" || m.Member.Name = "IsSome" || m.Member.Name = "IsNone") -> 

            let ext = 
                match m.Member.Name with
                | "Value" -> ExtProperty.Value
                | "IsSome" -> ExtProperty.IsSome
                | "IsNone" -> ExtProperty.IsNone
                | "HasValue" -> ExtProperty.HasValue
                | _ -> ExtProperty.NA

            tryGetMember m.Expression
            |> Option.map (fun pm -> pm, ext)
        | _ -> 
            tryGetMember exp
            |> Option.map (fun pm -> pm, ExtProperty.NA)

    /// A property/column in a record/table mapped to this query via a `for` or `join` clause.
    let (|MappedColumn|_|) (tables: TableMapping seq) (exp: Expression) = 
        match exp with
        | Property (p, ext) when tables |> Seq.exists (fun tbl -> tbl.IsInTable p) ->
            Some (p, ext)
        | _ -> 
            None

    /// A constant value or an expression that can be evaluated to a constant value.
    let (|Value|_|) (exp: Expression) =
        match exp with
        | Constant c -> Some c.Value
        // Do not try to evaluate QueryFunctions like `isIn`, `isNotIn`, etc.
        | Call c when c.Method.Module.Name <> "SqlHydra.Query.dll" -> 
            compileAndEvaluateExpression exp |> Some
        | _ -> None

    let (|AggregateColumn|_|) (exp: Expression) =
        match exp with
        | MethodCall m when List.contains m.Method.Name [ nameof minBy; nameof maxBy; nameof sumBy; nameof avgBy; nameof countBy; nameof avgByAs ] ->
            let aggType = m.Method.Name.Replace("By", "").Replace("As", "").ToUpper()
            match m.Arguments.[0] with
            | Property p -> Some (aggType, p)
            | _ -> notImplMsg "Invalid argument to aggregate function."
        | _ -> None

// ─── NormalizedExpression Patterns ───────────────────────────────────────────
// Active patterns on NormalizedExpression that delegate to existing Expression
// patterns for semantic checks. No semantic logic is duplicated.

open ExpressionNormalizer

[<AutoOpen>]
module NormalizedPatterns =

    /// Extracts alias by following NMemberAccess chain to NParameter.
    let rec nVisitAlias (nexp: NormalizedExpression) : string =
        match nexp with
        | NMemberAccess(inner, _) -> nVisitAlias inner
        | NParameter p -> p.Name
        | _ -> notImpl()

    /// Binary AND (And or AndAlso).
    let (|NBinaryAnd|_|) (nexp: NormalizedExpression) =
        match nexp with
        | NBinary(left, op, right) when op = ExpressionType.And || op = ExpressionType.AndAlso -> Some (left, right)
        | _ -> None

    /// Binary OR (Or or OrElse).
    let (|NBinaryOr|_|) (nexp: NormalizedExpression) =
        match nexp with
        | NBinary(left, op, right) when op = ExpressionType.Or || op = ExpressionType.OrElse -> Some (left, right)
        | _ -> None

    /// Binary comparison (=, <>, >, >=, <, <=). Returns (left, op, right).
    let (|NBinaryCompare|_|) (nexp: NormalizedExpression) =
        match nexp with
        | NBinary(left, op, right) ->
            match op with
            | ExpressionType.Equal | ExpressionType.NotEqual
            | ExpressionType.GreaterThan | ExpressionType.GreaterThanOrEqual
            | ExpressionType.LessThan | ExpressionType.LessThanOrEqual -> Some (left, op, right)
            | _ -> None
        | _ -> None

    /// Not / negation.
    let (|NNot|_|) (nexp: NormalizedExpression) =
        match nexp with
        | NUnary(ExpressionType.Not, operand) -> Some operand
        | _ -> None

    /// Bool member access.
    let (|NBoolMember|_|) (nexp: NormalizedExpression) =
        match nexp with
        | NMemberAccess(_, m) when m.Type = typeof<bool> -> Some m
        | _ -> None

    /// Bool constant.
    let (|NBoolConstant|_|) (nexp: NormalizedExpression) =
        match nexp with
        | NConstant(v, t) when t = typeof<bool> -> Some (v :?> bool)
        | _ -> None

    /// Property with extended info (Option/Nullable awareness).
    /// Delegates to the existing Property active pattern on the original Expression.
    let (|NProperty|_|) (nexp: NormalizedExpression) =
        match nexp with
        | NMemberAccess(_, m) ->
            match (m :> Expression) with
            | Property (p, ext) -> Some (p, ext)
            | _ -> None
        | NMethodCall(call, _) ->
            // Handle Option.Some/Nullable wrapping (e.g., Some c.ProductCategoryID)
            match (call :> Expression) with
            | Property (p, ext) -> Some (p, ext)
            | _ -> None
        | NUnary(ExpressionType.Convert, NMemberAccess(_, m)) ->
            // Handle implicit conversions wrapping a property
            match (m :> Expression) with
            | Property (p, ext) -> Some (p, ext)
            | _ -> None
        | _ -> None

    /// A constant value or an evaluable expression.
    /// Delegates to compileAndEvaluateExpression for non-constant evaluable expressions.
    let (|NValue|_|) (nexp: NormalizedExpression) =
        match nexp with
        | NConstant(v, _) -> Some v
        | NMethodCall(call, _) when call.Method.Module.Name <> "SqlHydra.Query.dll" ->
            compileAndEvaluateExpression (call :> Expression) |> Some
        | NMemberAccess(NConstant _, m) ->
            // Evaluable member access on a constant (e.g., captured variable from closure)
            compileAndEvaluateExpression (m :> Expression) |> Some
        | NUnary(ExpressionType.Convert, NConstant(v, t)) when t.IsPrimitive ->
            // Handle implicit conversions (e.g., int to int64)
            Some v
        | NUnknown exp when exp <> null ->
            try compileAndEvaluateExpression exp |> Some
            with _ -> None
        | _ -> None

    /// Aggregate column pattern (minBy, maxBy, sumBy, avgBy, countBy, avgByAs).
    let (|NAggregateColumn|_|) (nexp: NormalizedExpression) =
        match nexp with
        | NMethodCall(m, _) when List.contains m.Method.Name [ nameof minBy; nameof maxBy; nameof sumBy; nameof avgBy; nameof countBy; nameof avgByAs ] ->
            let aggType = m.Method.Name.Replace("By", "").Replace("As", "").ToUpper()
            match m.Arguments.[0] with
            | Property p -> Some (aggType, p)
            | _ -> notImplMsg "Invalid argument to aggregate function."
        | _ -> None

    /// List initializer — delegates to original ListInit pattern.
    let (|NListInit|_|) (nexp: NormalizedExpression) =
        match nexp with
        | NMethodCall(call, _) when call.Method.Name = "Cons" ->
            match (call :> Expression) with
            | ListInit values -> Some values
            | _ -> None
        | _ -> None

    /// Array initializer — delegates to original ArrayInit pattern.
    let (|NArrayInit|_|) (nexp: NormalizedExpression) =
        match nexp with
        | NUnknown exp ->
            match exp with
            | ArrayInit values -> Some values
            | _ -> None
        | _ -> None

let getComparison (expType: ExpressionType) =
    match expType with
    | ExpressionType.Equal -> "="
    | ExpressionType.NotEqual -> "<>"
    | ExpressionType.GreaterThan -> ">"
    | ExpressionType.GreaterThanOrEqual -> ">="
    | ExpressionType.LessThan -> "<"
    | ExpressionType.LessThanOrEqual -> "<="
    | _ -> notImplMsg "Unsupported comparison type"

let reverseComparison (expType: ExpressionType) =
    match expType with
    | ExpressionType.GreaterThan -> ExpressionType.LessThan
    | ExpressionType.GreaterThanOrEqual -> ExpressionType.LessThanOrEqual
    | ExpressionType.LessThan -> ExpressionType.GreaterThan
    | ExpressionType.LessThanOrEqual -> ExpressionType.GreaterThanOrEqual
    | _ -> expType


let getReverseComparison = getComparison << reverseComparison
    
let visitAlias (exp: Expression) =
    let rec visit (exp: Expression) =
        match exp with
        | Member m -> visit m.Expression
        | Parameter p -> p.Name
        | _ -> notImpl()
    visit exp

/// Converts a SQL function MethodCall expression to a SQL fragment string.
/// Example: LEN(p.FirstName) -> "LEN({p}.{FirstName})"
let rec visitSqlFn (qualifyColumn: string -> MemberInfo -> string) (exp: Expression) : string =
    match exp with
    | MethodCall m ->
        let fnName = m.Method.Name
        let args =
            m.Arguments
            |> Seq.map (fun arg ->
                match arg with
                | Member mem ->
                    let alias = visitAlias mem.Expression
                    qualifyColumn alias mem.Member
                | Constant c when c.Value = null ->
                    "NULL"
                | Constant c when c.Type = typeof<string> ->
                    $"'{c.Value}'"
                | Constant c ->
                    sprintf "%O" c.Value
                | MethodCall _ as nested ->
                    // Handle nested function calls
                    visitSqlFn qualifyColumn nested
                | _ ->
                    notImplMsg $"Unsupported argument type in SQL function: {arg.NodeType}"
            )
            |> String.concat ", "
        $"{fnName}({args})"
    | _ ->
        notImplMsg $"Expected a method call expression but got: {exp.NodeType}"

/// Delegates to existing visitSqlFn by extracting the original MethodCallExpression.
let nVisitSqlFn (qualifyColumn: string -> MemberInfo -> string) (nexp: NormalizedExpression) : string =
    match nexp with
    | NMethodCall(m, _) -> visitSqlFn qualifyColumn (m :> Expression)
    | _ -> notImplMsg $"Expected NMethodCall for SQL function"

let visitWhere<'T> (tables: TableMapping seq) (filter: Expression<Func<'T, bool>>) (qualifyColumn: string -> MemberInfo -> string) =
    let (|NColumn|_|) (nexp: NormalizedExpression) =
        match nexp with
        | NProperty (p, ext) when tables |> Seq.exists (fun tbl -> tbl.IsInTable p) -> Some (p, ext)
        | _ -> None

    /// Evaluate a NormalizedExpression to a runtime value.
    let nEvaluate (nexp: NormalizedExpression) =
        match nexp with
        | NValue v -> v
        | NMemberAccess(_, m) -> compileAndEvaluateExpression (m :> Expression)
        | NMethodCall(m, _) -> compileAndEvaluateExpression (m :> Expression)
        | NUnknown exp -> compileAndEvaluateExpression exp
        | _ -> notImplMsg $"Unable to evaluate expression: {nexp}"

    let rec visit (nexp: NormalizedExpression) (query: Query) : Query =
        match nexp with
        | NMethodCall(m, args) when List.contains m.Method.Name [ nameof isIn; nameof isNotIn; nameof op_BarEqualsBar; nameof op_BarLessGreaterBar ] ->
            let filter : (string * seq<obj>) -> Query =
                match m.Method.Name with
                | nameof isIn | nameof op_BarEqualsBar -> query.WhereIn
                | _ -> query.WhereNotIn

            match args.[0], args.[1] with
            | NColumn (p, _), NMethodCall(subqueryExpr, _) when subqueryExpr.Method.Name = nameof subqueryMany ->
                let subqueryConst = match subqueryExpr.Arguments.[0] with | Constant c -> c | _ -> notImpl()
                let alias = visitAlias p.Expression
                let fqCol = qualifyColumn alias p.Member
                let selectSubquery = subqueryConst.Value :?> SelectQuery
                match m.Method.Name with
                | nameof isIn | nameof op_BarEqualsBar -> query.WhereIn(fqCol, selectSubquery.ToKataQuery())
                | _ -> query.WhereNotIn(fqCol, selectSubquery.ToKataQuery())
            | NColumn (p, _), NListInit values ->
                let queryParameters =
                    values
                    |> Seq.map (KataUtils.getQueryParameterForValue p.Member)
                    |> Seq.toArray
                let alias = visitAlias p.Expression
                let fqCol = qualifyColumn alias p.Member
                filter(fqCol, queryParameters)
            | NColumn (p, _), NArrayInit values ->
                let queryParameters =
                    values
                    |> Seq.map (KataUtils.getQueryParameterForValue p.Member)
                    |> Seq.toArray
                let alias = visitAlias p.Expression
                let fqCol = qualifyColumn alias p.Member
                filter(fqCol, queryParameters)
            | NColumn (p, _), NValue value ->
                let queryParameters =
                    (value :?> System.Collections.IEnumerable)
                    |> Seq.cast<obj>
                    |> Seq.map (KataUtils.getQueryParameterForValue p.Member)
                    |> Seq.toArray
                let alias = visitAlias p.Expression
                let fqCol = qualifyColumn alias p.Member
                filter(fqCol, queryParameters)
            | NColumn _, NMethodCall(c, _) when c.Method.Name = "CreateSequence" ->
                notImplMsg "Unable to unwrap sequence expression. Please use a list or array instead."
            | _ -> notImpl()

        // like / notLike fns
        | NMethodCall(m, args) when List.contains m.Method.Name [ nameof like; nameof notLike; nameof op_EqualsPercent; nameof op_LessGreaterPercent ] ->
            match args.[0], args.[1] with
            | NColumn (p, _), NValue value ->
                let pattern = string value
                let alias = visitAlias p.Expression
                let fqCol = qualifyColumn alias p.Member
                match m.Method.Name with
                | nameof like | nameof op_EqualsPercent -> query.WhereLike(fqCol, pattern, false)
                | _ -> query.WhereNotLike(fqCol, pattern, false)
            | _ -> notImpl()

        // isNull / isNotNull
        | NMethodCall(m, args) when List.contains m.Method.Name [ nameof isNullValue; "IsNull"; nameof isNotNullValue ] ->
            match args.[0] with
            | NColumn (p, _) ->
                let alias = visitAlias p.Expression
                let fqCol = qualifyColumn alias p.Member
                if m.Method.Name = nameof isNullValue || m.Method.Name = "IsNull"
                then query.WhereNull(fqCol)
                else query.WhereNotNull(fqCol)
            | _ -> notImpl()

        // areEqual / notEqual
        | NMethodCall(m, args) when List.contains m.Method.Name [ nameof areEqual; nameof notEqual ] ->
            match args.[0], args.[1] with
            | NColumn (p1, _), NColumn (p2, _) ->
                let alias1 = visitAlias p1.Expression
                let fqCol1 = qualifyColumn alias1 p1.Member
                let alias2 = visitAlias p2.Expression
                let fqCol2 = qualifyColumn alias2 p2.Member
                let comparison = if m.Method.Name = nameof areEqual then "=" else "<>"
                query.WhereColumns(fqCol1, comparison, fqCol2)
            | NColumn (p, _), NValue value | NValue value, NColumn (p, _) ->
                let alias1 = visitAlias p.Expression
                let fqCol1 = qualifyColumn alias1 p.Member
                let queryParameter = KataUtils.getQueryParameterForValue p.Member value
                let comparison = if m.Method.Name = nameof areEqual then "=" else "<>"
                query.Where(fqCol1, comparison, queryParameter)
            | _ -> notImpl()

        // Nullable / Option .HasValue / .IsSome
        | NMemberAccess(_, bm) & NColumn (p, ext) when
            bm.Type = typeof<bool>
            && p.Type |> isOptionOrNullableType
            && (ext = ExtProperty.HasValue || ext = ExtProperty.IsSome) ->
            let alias = visitAlias p.Expression
            let m = tryGetMember p
            let fqCol = qualifyColumn alias m.Value.Member
            query.WhereNotNull(fqCol)

        | NNot (NMemberAccess(_, bm) & NColumn (p, ext)) when
            bm.Type = typeof<bool>
            && p.Type |> isOptionOrNullableType
            && (ext = ExtProperty.HasValue || ext = ExtProperty.IsSome) ->
            let alias = visitAlias p.Expression
            let m = tryGetMember p
            let fqCol = qualifyColumn alias m.Value.Member
            query.WhereNull(fqCol)

        // Option.IsNone
        | NMemberAccess(_, bm) & NColumn (p, ext) when
            bm.Type = typeof<bool>
            && p.Type |> isOptionType
            && ext = ExtProperty.IsNone ->
            let alias = visitAlias p.Expression
            let m = tryGetMember p
            let fqCol = qualifyColumn alias m.Value.Member
            query.WhereNull(fqCol)

        | NNot (NMemberAccess(_, bm) & NColumn (p, ext)) when
            bm.Type = typeof<bool>
            && p.Type |> isOptionType
            && ext = ExtProperty.IsNone ->
            let alias = visitAlias p.Expression
            let m = tryGetMember p
            let fqCol = qualifyColumn alias m.Value.Member
            query.WhereNotNull(fqCol)

        // Bool column `where user.IsEnabled`
        | NMemberAccess(_, bm) & NColumn (p, _) when bm.Type = typeof<bool> ->
            let alias = visitAlias p.Expression
            let fqCol = qualifyColumn alias p.Member
            query.Where(fqCol, "=", true)

        | NNot (NMemberAccess(_, bm) & NColumn (p, _)) when bm.Type = typeof<bool> ->
            let alias = visitAlias p.Expression
            let fqCol = qualifyColumn alias p.Member
            query.Where(fqCol, "=", false)

        | NNot operand ->
            let operand = visit operand (Query())
            query.WhereNot(fun q -> operand)

        | NBinaryAnd(left, right) ->
            match left with
            | NValue enabled ->
                if enabled :?> bool
                then visit right (Query())
                else query
            | _ ->
                let lt = visit left (Query())
                let rt = visit right (Query())
                query.Where(fun q -> lt).Where(fun q -> rt)

        | NBinaryOr(left, right) ->
            match left with
            | NValue enabled ->
                if enabled :?> bool
                then visit right (Query())
                else query
            | _ ->
                let lt = visit left (Query())
                let rt = visit right (Query())
                query.OrWhere(fun q -> lt).OrWhere(fun q -> rt)

        | NBinaryCompare(left, op, right) ->
            let comparison = getComparison op
            match left, right with

            // Property to subquery
            | NColumn (p1, _), NMethodCall(subqueryExpr, _) when subqueryExpr.Method.Name = nameof subqueryOne ->
                let subqueryConst = match subqueryExpr.Arguments.[0] with | Constant c -> c | _ -> notImpl()
                let selectSubquery = subqueryConst.Value :?> SelectQuery
                let alias = visitAlias p1.Expression
                let fqCol = qualifyColumn alias p1.Member
                query.Where(fqCol, comparison, selectSubquery.ToKataQuery())

            // Col to col
            | NColumn (p1, _), NColumn (p2, _) ->
                let lt =
                    let alias = visitAlias p1.Expression
                    qualifyColumn alias p1.Member
                let rt =
                    let alias = visitAlias p2.Expression
                    qualifyColumn alias p2.Member
                query.WhereColumns(lt, comparison, rt)

            // Column = null
            | NColumn (p, _), NConstant(null, _) | NConstant(null, _), NColumn (p, _) when op = ExpressionType.Equal ->
                let alias = visitAlias p.Expression
                let fqCol = qualifyColumn alias p.Member
                query.WhereNull(fqCol)

            // Column <> null
            | NColumn (p, _), NConstant(null, _) | NConstant(null, _), NColumn (p, _) when op = ExpressionType.NotEqual ->
                let alias = visitAlias p.Expression
                let fqCol = qualifyColumn alias p.Member
                query.WhereNotNull(fqCol)

            // Option.IsSome / Nullable.HasValue null check (Equal)
            | NColumn (p, ext), NBoolConstant value | NBoolConstant value, NColumn (p, ext) when
                p.Type |> isOptionOrNullableType
                && (ext = ExtProperty.HasValue || ext = ExtProperty.IsSome)
                && op = ExpressionType.Equal ->
                let alias = visitAlias p.Expression
                let m = tryGetMember p
                let fqCol = qualifyColumn alias m.Value.Member
                match value with
                | true -> query.WhereNotNull(fqCol)
                | false -> query.WhereNull(fqCol)

            // Option.IsSome / Nullable.HasValue null check (NotEqual)
            | NColumn (p, ext), NBoolConstant value | NBoolConstant value, NColumn (p, ext) when
                p.Type |> isOptionOrNullableType
                && (ext = ExtProperty.HasValue || ext = ExtProperty.IsSome)
                && op = ExpressionType.NotEqual ->
                let alias = visitAlias p.Expression
                let fqCol = qualifyColumn alias p.Member
                match value with
                | true -> query.WhereNull(fqCol)
                | false -> query.WhereNotNull(fqCol)

            // Nullable.Value comparisons
            | NColumn (p, ext), NValue value | NValue value, NColumn (p, ext) when
                p.Type |> isOptionOrNullableType
                && ext = ExtProperty.Value ->
                let queryParameter = KataUtils.getQueryParameterForValue p.Member value
                let alias = visitAlias p.Expression
                let m = tryGetMember p
                let fqCol = qualifyColumn alias m.Value.Member
                query.Where(fqCol, comparison, queryParameter)

            | NColumn (p, _), _ ->
                let value = nEvaluate right
                let alias = visitAlias p.Expression
                let fqCol = qualifyColumn alias p.Member
                match value with
                | null when comparison = "=" -> query.WhereNull(fqCol)
                | null when comparison = "<>" -> query.WhereNotNull(fqCol)
                | _ ->
                    let queryParameter = KataUtils.getQueryParameterForValue p.Member value
                    query.Where(fqCol, comparison, queryParameter)

            | _, NColumn (p, _) ->
                let value = nEvaluate left
                let reversedComparison = getReverseComparison op
                let alias = visitAlias p.Expression
                let fqCol = qualifyColumn alias p.Member
                match value with
                | null when reversedComparison = "=" -> query.WhereNull(fqCol)
                | null when reversedComparison = "<>" -> query.WhereNotNull(fqCol)
                | _ ->
                    let queryParameter = KataUtils.getQueryParameterForValue p.Member value
                    query.Where(fqCol, reversedComparison, queryParameter)

            // SQL function compared to value
            | NMethodCall _, NValue value ->
                let sqlFragment = nVisitSqlFn qualifyColumn left
                query.WhereRaw($"{sqlFragment} {comparison} ?", [| value |])

            // Value compared to SQL function
            | NValue value, NMethodCall _ ->
                let sqlFragment = nVisitSqlFn qualifyColumn right
                let reversedComparison = getReverseComparison op
                query.WhereRaw($"{sqlFragment} {reversedComparison} ?", [| value |])

            // SQL function compared to column
            | NMethodCall _, NColumn (p, _) ->
                let sqlFragment = nVisitSqlFn qualifyColumn left
                let alias = visitAlias p.Expression
                let fqCol = qualifyColumn alias p.Member
                query.WhereRaw($"{sqlFragment} {comparison} {fqCol}")

            // Column compared to SQL function
            | NColumn (p, _), NMethodCall _ ->
                let alias = visitAlias p.Expression
                let fqCol = qualifyColumn alias p.Member
                let sqlFragment = nVisitSqlFn qualifyColumn right
                query.WhereRaw($"{fqCol} {comparison} {sqlFragment}")

            // SQL function compared to SQL function
            | NMethodCall _, NMethodCall _ ->
                let sqlFragment1 = nVisitSqlFn qualifyColumn left
                let sqlFragment2 = nVisitSqlFn qualifyColumn right
                query.WhereRaw($"{sqlFragment1} {comparison} {sqlFragment2}")

            // Anti-join pattern: `where (d = None)` or `where (d <> None)` on a left-joined table parameter
            // F# compiles `None` as a method call (FSharpOption.get_None()), not as Constant null
            | Parameter p, _ | _, Parameter p when
                p.Type |> isOptionType ->
                let otherSide = if (x.Left :> Expression).NodeType = ExpressionType.Parameter then x.Right else x.Left
                let evaluatedValue = compileAndEvaluateExpression otherSide
                match evaluatedValue with
                | null ->
                    // Get the inner record type from Option<T>
                    let innerType = p.Type.GetGenericArguments().[0]
                    // Get the first record field to use for the IS NULL / IS NOT NULL check
                    let firstField = FSharp.Reflection.FSharpType.GetRecordFields(innerType).[0]
                    let fqCol = qualifyColumn p.Name (firstField :> MemberInfo)
                    match exp.NodeType with
                    | ExpressionType.Equal -> query.WhereNull(fqCol)
                    | ExpressionType.NotEqual -> query.WhereNotNull(fqCol)
                    | _ -> notImplMsg "Unsupported comparison on left-joined table parameter"
                | _ -> notImplMsg "Left-joined table parameter can only be compared to None"

            | NValue _, NValue _ ->
                notImplMsg("Value to value comparisons are not currently supported. Ex: where (1 = 1)")
            | _ ->
                notImpl()

        | _ ->
            notImplMsg $"Unsupported expression type in where clause: {nexp}"

    visit (ExpressionNormalizer.toNormalizedExpression (filter :> Expression)) (Query())

let visitHaving<'T> (tables: TableMapping seq) (filter: Expression<Func<'T, bool>>) (qualifyColumn: string -> MemberInfo -> string) =
    let (|NColumn|_|) (nexp: NormalizedExpression) =
        match nexp with
        | NProperty (p, ext) when tables |> Seq.exists (fun tbl -> tbl.IsInTable p) -> Some (p, ext)
        | _ -> None

    let rec visit (nexp: NormalizedExpression) (query: Query) : Query =
        match nexp with
        | NNot operand ->
            let operand = visit operand (Query())
            query.HavingNot(fun q -> operand)
        | NMethodCall(m, args) when List.contains m.Method.Name [ nameof isIn; nameof isNotIn; nameof op_BarEqualsBar; nameof op_BarLessGreaterBar ] ->
            let filter : (string * seq<obj>) -> Query =
                match m.Method.Name with
                | nameof isIn | nameof op_BarEqualsBar -> query.HavingIn
                | _ -> query.HavingNotIn

            match args.[0], args.[1] with
            | NColumn (p, _), NMethodCall(subqueryExpr, _) when subqueryExpr.Method.Name = nameof subqueryMany ->
                let subqueryConst = match subqueryExpr.Arguments.[0] with | Constant c -> c | _ -> notImpl()
                let alias = visitAlias p.Expression
                let fqCol = qualifyColumn alias p.Member
                let selectSubquery = subqueryConst.Value :?> SelectQuery
                match m.Method.Name with
                | nameof isIn | nameof op_BarEqualsBar -> query.HavingIn(fqCol, selectSubquery.ToKataQuery())
                | _ -> query.HavingNotIn(fqCol, selectSubquery.ToKataQuery())
            | NColumn (p, _), NListInit values ->
                let queryParameters =
                    values
                    |> Seq.map (KataUtils.getQueryParameterForValue p.Member)
                    |> Seq.toArray
                let alias = visitAlias p.Expression
                let fqCol = qualifyColumn alias p.Member
                filter(fqCol, queryParameters)
            | NColumn (p, _), NArrayInit values ->
                let queryParameters =
                    values
                    |> Seq.map (KataUtils.getQueryParameterForValue p.Member)
                    |> Seq.toArray
                let alias = visitAlias p.Expression
                let fqCol = qualifyColumn alias p.Member
                filter(fqCol, queryParameters)
            | NColumn (p, _), NValue value ->
                let queryParameters =
                    (value :?> System.Collections.IEnumerable)
                    |> Seq.cast<obj>
                    |> Seq.map (KataUtils.getQueryParameterForValue p.Member)
                    |> Seq.toArray
                let alias = visitAlias p.Expression
                let fqCol = qualifyColumn alias p.Member
                filter(fqCol, queryParameters)
            | NColumn _, NMethodCall(c, _) when c.Method.Name = "CreateSequence" ->
                notImplMsg "Unable to unwrap sequence expression. Please use a list or array instead."
            | _ -> notImpl()
        | NMethodCall(m, args) when List.contains m.Method.Name [ nameof like; nameof notLike; nameof op_EqualsPercent; nameof op_LessGreaterPercent ] ->
            match args.[0], args.[1] with
            | NColumn (p, _), NValue value ->
                let pattern = string value
                match m.Method.Name with
                | nameof like | nameof op_EqualsPercent ->
                    let alias = visitAlias p.Expression
                    let fqCol = qualifyColumn alias p.Member
                    query.HavingLike(fqCol, pattern, false)
                | _ ->
                    let alias = visitAlias p.Expression
                    let fqCol = qualifyColumn alias p.Member
                    query.HavingNotLike(fqCol, pattern, false)
            | _ -> notImpl()
        | NMethodCall(m, args) when m.Method.Name = nameof isNullValue || m.Method.Name = nameof isNotNullValue ->
            match args.[0] with
            | NColumn (p, _) ->
                let alias = visitAlias p.Expression
                let fqCol = qualifyColumn alias p.Member
                if m.Method.Name = nameof isNullValue
                then query.HavingNull(fqCol)
                else query.HavingNotNull(fqCol)
            | _ -> notImpl()
        | NMethodCall(m, args) when List.contains m.Method.Name [ nameof minBy; nameof maxBy; nameof sumBy; nameof avgBy; nameof countBy; nameof avgByAs ] ->
            visit args.[0] query
        | NBinaryAnd(left, right) ->
            let lt = visit left (Query())
            let rt = visit right (Query())
            query.Having(fun q -> lt).Having(fun q -> rt)
        | NBinaryOr(left, right) ->
            let lt = visit left (Query())
            let rt = visit right (Query())
            query.OrHaving(fun q -> lt).OrHaving(fun q -> rt)
        | NBinaryCompare(left, op, right) ->
            let comparison = getComparison op
            match left, right with
            | NColumn (p1, _), NMethodCall(subqueryExpr, _) when subqueryExpr.Method.Name = nameof subqueryOne ->
                let subqueryConst = match subqueryExpr.Arguments.[0] with | Constant c -> c | _ -> notImpl()
                let selectSubquery = subqueryConst.Value :?> SelectQuery
                let alias = visitAlias p1.Expression
                let fqCol = qualifyColumn alias p1.Member
                query.Having(fqCol, comparison, selectSubquery.ToKataQuery())
            | NAggregateColumn (aggType, (p1, _)), NColumn (p2, _) ->
                let lt =
                    let alias = visitAlias p1.Expression
                    qualifyColumn alias p1.Member
                let rt =
                    let alias = visitAlias p2.Expression
                    qualifyColumn alias p2.Member
                query.HavingRaw($"{aggType}({lt}) {comparison} {rt}")
            | NAggregateColumn (aggType, (p, _)), NValue value ->
                let alias = visitAlias p.Expression
                let lt = qualifyColumn alias p.Member
                query.HavingRaw($"{aggType}({lt}) {comparison} ?", [value])
            | NColumn (p1, _), NColumn (p2, _) ->
                let lt =
                    let alias = visitAlias p1.Expression
                    qualifyColumn alias p1.Member
                let rt =
                    let alias = visitAlias p2.Expression
                    qualifyColumn alias p2.Member
                query.HavingColumns(lt, comparison, rt)
            | NColumn (p, _), NValue value ->
                match op, value with
                | ExpressionType.Equal, null ->
                    let alias = visitAlias p.Expression
                    query.WhereNull(qualifyColumn alias p.Member)
                | ExpressionType.NotEqual, null ->
                    let alias = visitAlias p.Expression
                    query.WhereNotNull(qualifyColumn alias p.Member)
                | _ ->
                    let queryParameter = KataUtils.getQueryParameterForValue p.Member value
                    let alias = visitAlias p.Expression
                    query.Where(qualifyColumn alias p.Member, comparison, queryParameter)
            | NValue _, NValue _ ->
                notImplMsg("Value to value comparisons are not currently supported. Ex: having (1 = 1)")
            | _ ->
                notImpl()

        | _ ->
            notImplMsg $"Unsupported expression type in having clause: {nexp}"

    visit (ExpressionNormalizer.toNormalizedExpression (filter :> Expression)) (Query())

/// Returns a list of one or more fully qualified column names: ["{schema}.{table}.{column}"]
let visitPropertiesSelector<'T, 'Prop> (propertySelector: Expression<Func<'T, 'Prop>>) (qualifyColumn: string -> MemberInfo -> string) =
    let rec visit (nexp: NormalizedExpression) : string list =
        match nexp with
        | NNew(_, args) ->
            args |> List.collect visit
        | NMemberAccess(inner, m) ->
            let alias = nVisitAlias inner
            let column = qualifyColumn alias m.Member
            [column]
        | _ -> notImpl()

    visit (ExpressionNormalizer.toNormalizedExpression (propertySelector :> Expression))

type OrderBy =
    | OrderByColumn of tableAlias: string * MemberInfo
    | OrderByAggregateColumn of aggregateType: string * tableAlias: string * MemberInfo
    | OrderByIgnored

/// Returns a column MemberInfo.
let visitOrderByPropertySelector<'T, 'Prop> (propertySelector: Expression<Func<'T, 'Prop>>) =
    let rec visit (nexp: NormalizedExpression) : OrderBy =
        match nexp with
        | NMethodCall(m, args) when m.Method.Name = nameof op_HatHat ->
            // ^^ operator conditionally adds property to order by clause
            match args.[0], args.[1] with
            | NValue enabled, NProperty (p, _) ->
                if enabled :?> bool then
                    let alias = visitAlias p.Expression
                    OrderByColumn (alias, p.Member)
                else
                    OrderByIgnored
            | _ ->
                notImpl()
        | NAggregateColumn (aggType, (p, _)) ->
            let alias = visitAlias p.Expression
            OrderByAggregateColumn (aggType, alias, p.Member)
        | NMemberAccess(inner, m) ->
            if m.Member.DeclaringType |> isOptionOrNullableType then
                visit inner
            else
                let alias = visitAlias m.Expression
                OrderByColumn (alias, m.Member)
        | NProperty (p, _) ->
            let alias = visitAlias p.Expression
            OrderByColumn (alias, p.Member)
        | _ -> notImpl()

    visit (ExpressionNormalizer.toNormalizedExpression (propertySelector :> Expression))

type JoinedPropertyInfo = 
    {
        Alias: string
        Member: MemberInfo
    }

/// Returns one or more column members
let visitJoin<'T, 'Prop> (propertySelector: Expression<Func<'T, 'Prop>>) =
    let rec visit (nexp: NormalizedExpression) : JoinedPropertyInfo list =
        match nexp with
        | NNew(_, args) ->
            args |> List.collect visit
        | NMethodCall(m, args) when m.Method.Name = "Some" ->
            // Option.Some wrapping — visit the inner argument
            visit args.[0]
        | NMemberAccess(inner, m) ->
            if m.Member.DeclaringType |> isOptionOrNullableType
            then visit inner
            else
                let alias = visitAlias m.Expression
                [ { Alias = alias; Member = m.Member } ]
        | NProperty (p, _) ->
            let alias = visitAlias p.Expression
            [ { Alias = alias; Member = p.Member } ]
        | _ -> notImpl()

    visit (ExpressionNormalizer.toNormalizedExpression (propertySelector :> Expression))

/// Returns a column MemberInfo.
let visitPropertySelector<'T, 'Prop> (propertySelector: Expression<Func<'T, 'Prop>>) =
    let rec visit (nexp: NormalizedExpression) : MemberInfo =
        match nexp with
        | NMemberAccess(inner, m) ->
            if m.Member.DeclaringType |> isOptionOrNullableType
            then visit inner
            else m.Member
        | NProperty (p, _) -> p.Member
        | _ -> notImpl()

    visit (ExpressionNormalizer.toNormalizedExpression (propertySelector :> Expression))

type Selection =
    | SelectedTable of tableAlias: string * tableType: Type
    | SelectedColumn of tableAlias: string * column: string * columnType: Type * isOpt: bool * isNullable: bool
    | SelectedExpression of sqlFragment: string


/// Visits a join predicate expression and builds SqlKata Join.On() calls.
/// Used by the `on'` operation to support predicate-style joins.
let visitJoinPredicate<'T> (tables: TableMapping seq) (predicate: Expression<Func<'T, bool>>) (qualifyColumn: string -> MemberInfo -> string) =
    /// A column/property on a mapped table/record.
    let (|NColumn|_|) (nexp: NormalizedExpression) =
        match nexp with
        | NProperty (p, ext) when tables |> Seq.exists (fun tbl -> tbl.IsInTable p) -> Some (p, ext)
        | _ -> None

    let rec visit (nexp: NormalizedExpression) (j: SqlKata.Join) : SqlKata.Join =
        match nexp with
        | NBinaryAnd(left, right) ->
            let j' = visit left j
            visit right j'
        | NBinaryOr(left, right) ->
            let leftJoin = visit left (SqlKata.Join())
            let rightJoin = visit right (SqlKata.Join())
            j.Where(fun _ -> leftJoin).OrWhere(fun _ -> rightJoin)
        | NBinaryCompare(left, op, right) ->
            let comparison = getComparison op
            match left, right with
            // Handle col to col comparisons (the primary join case)
            | NColumn (p1, _), NColumn (p2, _) ->
                let lt =
                    let alias = visitAlias p1.Expression
                    qualifyColumn alias p1.Member
                let rt =
                    let alias = visitAlias p2.Expression
                    qualifyColumn alias p2.Member
                j.On(lt, rt, comparison)

            // Handle column to value comparisons
            | NColumn (p, _), NValue value ->
                let alias = visitAlias p.Expression
                let fqCol = qualifyColumn alias p.Member
                match value with
                | null when comparison = "=" -> j.WhereNull(fqCol)
                | null when comparison = "<>" -> j.WhereNotNull(fqCol)
                | _ ->
                    let queryParameter = KataUtils.getQueryParameterForValue p.Member value
                    j.Where(fqCol, comparison, queryParameter)

            // Handle value to column comparisons (reversed)
            | NValue value, NColumn (p, _) ->
                let alias = visitAlias p.Expression
                let fqCol = qualifyColumn alias p.Member
                let reversedComparison = getReverseComparison op
                match value with
                | null when reversedComparison = "=" -> j.WhereNull(fqCol)
                | null when reversedComparison = "<>" -> j.WhereNotNull(fqCol)
                | _ ->
                    let queryParameter = KataUtils.getQueryParameterForValue p.Member value
                    j.Where(fqCol, reversedComparison, queryParameter)

            // Nullable.Value / Option.Value comparisons
            | NColumn (p, ext), _ when ext = ExtProperty.Value ->
                let value =
                    match right with
                    | NValue v -> v
                    | NMemberAccess(_, m) -> compileAndEvaluateExpression (m :> Expression)
                    | NUnknown exp -> compileAndEvaluateExpression exp
                    | _ -> notImplMsg "Unable to evaluate join predicate value"
                let alias = visitAlias p.Expression
                let m = tryGetMember p
                let fqCol = qualifyColumn alias m.Value.Member
                match value with
                | null when comparison = "=" -> j.WhereNull(fqCol)
                | null when comparison = "<>" -> j.WhereNotNull(fqCol)
                | _ ->
                    let queryParameter = KataUtils.getQueryParameterForValue p.Member value
                    j.Where(fqCol, comparison, queryParameter)

            | _ ->
                notImplMsg $"Unsupported join predicate comparison: {op}"
        | _ ->
            notImplMsg $"Unsupported join predicate expression: {nexp}"

    fun (j: SqlKata.Join) -> visit (ExpressionNormalizer.toNormalizedExpression (predicate :> Expression)) j

/// Returns a list of one or more fully qualified table names: ["{schema}.{table}"]
let visitSelect<'T, 'Prop> (propertySelector: Expression<Func<'T, 'Prop>>) =
    let rec visit (nexp: NormalizedExpression) : Selection list =
        match nexp with
        | NMethodCall(m, args) when m.Method.Name = "Some" ->
            visit args.[0]
        // Handle direct OptionModule.Map calls
        | NMethodCall(m, args) when m.Method.Name = "Map"
            && m.Method.DeclaringType <> null
            && m.Method.DeclaringType.Name = "OptionModule"
            && args.Length = 2 ->
            let source = m.Arguments.[1] // original Expression for visitAlias
            let mappingArg = m.Arguments.[0]
            let rec extractMember (exp: Expression) =
                match exp with
                | :? LambdaExpression as lam -> extractMember lam.Body
                | :? UnaryExpression as u when u.NodeType = ExpressionType.Convert -> extractMember u.Operand
                | Member m -> Some m
                | _ -> None
            match extractMember mappingArg with
            | Some memberExp ->
                let alias = visitAlias source
                [ SelectedColumn (alias, memberExp.Member.Name, memberExp.Type, true, false) ]
            | None -> notImplMsg $"Unsupported Option.map mapping expression: {mappingArg.NodeType}"
        | NMethodCall(m, _) when m.Method.Name = "op_PipeRight" && m.Arguments.Count = 2 ->
            // Handle: r |> Option.map _.ColumnA
            // Use original Expression arguments for the complex Option.map lambda extraction
            let source = m.Arguments.[0]
            let pipeArg = m.Arguments.[1]
            let rec findOptionMapLambda (exp: Expression) =
                match exp with
                | :? MethodCallExpression as invoke when invoke.Method.Name = "Invoke" ->
                    match invoke.Arguments.[0] with
                    | :? MethodCallExpression as toFF when toFF.Method.Name = "ToFSharpFunc" ->
                        match toFF.Arguments.[0] with
                        | :? LambdaExpression as mapLam -> Some mapLam
                        | _ -> None
                    | _ -> None
                | :? MethodCallExpression as mc when
                    mc.Method.Name = "Map"
                    && mc.Method.DeclaringType <> null
                    && mc.Method.DeclaringType.Name = "OptionModule"
                    && mc.Arguments.Count = 2 ->
                    match mc.Arguments.[0] with
                    | :? LambdaExpression as mapLam -> Some mapLam
                    | :? MethodCallExpression as toFF when toFF.Method.Name = "ToFSharpFunc" ->
                        match toFF.Arguments.[0] with
                        | :? LambdaExpression as mapLam -> Some mapLam
                        | _ -> None
                    | _ -> None
                | :? MethodCallExpression as mc when mc.Method.Name = "ToFSharpFunc" && mc.Arguments.Count = 1 ->
                    match mc.Arguments.[0] with
                    | :? LambdaExpression as lam -> findOptionMapLambda lam.Body
                    | _ -> None
                | :? LambdaExpression as lam -> findOptionMapLambda lam.Body
                | _ -> None
            let rec containsOptionMap (exp: Expression) =
                match exp with
                | :? MethodCallExpression as mc ->
                    mc.Method.Name = "Map" && mc.Method.DeclaringType <> null && mc.Method.DeclaringType.Name = "OptionModule"
                    || mc.Arguments |> Seq.exists containsOptionMap
                    || (mc.Object <> null && containsOptionMap mc.Object)
                | :? LambdaExpression as lam -> containsOptionMap lam.Body
                | _ -> false
            if containsOptionMap pipeArg then
                match findOptionMapLambda pipeArg with
                | Some mapLam ->
                    match mapLam.Body with
                    | Member memberExp ->
                        let alias = visitAlias source
                        [ SelectedColumn (alias, memberExp.Member.Name, memberExp.Type, true, false) ]
                    | _ -> notImplMsg $"Unsupported Option.map lambda body: {mapLam.Body.NodeType}"
                | None -> notImplMsg $"Could not extract mapping lambda from Option.map expression"
            else
                let qualifyCol alias (mem: MemberInfo) = $"{{%s{alias}}}.{{%s{mem.Name}}}"
                let sqlFragment = visitSqlFn qualifyCol (m :> Expression)
                [ SelectedExpression sqlFragment ]
        | NAggregateColumn (aggType, (p, _)) ->
            let alias = visitAlias p.Expression
            let fqCol = $"{{%s{alias}}}.{{%s{p.Member.Name}}}"
            [ SelectedExpression $"{aggType}({fqCol})" ]
        | NMethodCall(m, _) ->
            let qualifyCol alias (mem: MemberInfo) = $"{{%s{alias}}}.{{%s{mem.Name}}}"
            let sqlFragment = visitSqlFn qualifyCol (m :> Expression)
            [ SelectedExpression sqlFragment ]
        | NNew(_, args) ->
            args |> List.collect visit
        | NParameter p ->
            [ SelectedTable (p.Name, p.Type) ]
        | NMemberAccess(inner, m) ->
            if m.Member.DeclaringType |> isOptionOrNullableType then
                visit inner
            else
                let isOptional, isNullable =
                    if m.Type.IsGenericType && m.Type.GetGenericTypeDefinition() = typedefof<Option<_>> then true, false
                    elif m.Type.IsGenericType && m.Type.GetGenericTypeDefinition() = typedefof<Nullable<_>> then false, true
                    else false, false
                let alias = visitAlias m.Expression
                [ SelectedColumn (alias, m.Member.Name, m.Type, isOptional, isNullable) ]
        | _ ->
            notImpl()

    visit (ExpressionNormalizer.toNormalizedExpression (propertySelector :> Expression))
