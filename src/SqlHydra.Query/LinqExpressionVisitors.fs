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
        | Member m when m.Expression.NodeType = ExpressionType.Call ->
            // Handle tuple destructuring from correlate/IsLikeZip (get_Item1, get_Item2, etc.)
            match m.Expression with
            | MethodCall mc when mc.Method.Name.StartsWith("get_Item") -> Some m
            | _ -> None
        | Member m when m.Expression.NodeType = ExpressionType.Block ->
            // Handle BlockExpression wrapping from tuple destructuring
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
        | MethodCall m when List.contains m.Method.Name [ nameof minBy; nameof maxBy; nameof sumBy; nameof avgBy; nameof countBy; nameof avgByAs; nameof countDistinct ] ->
            let aggType =
                if m.Method.Name = nameof countDistinct then "COUNTDISTINCT"
                else m.Method.Name.Replace("By", "").Replace("As", "").ToUpper()
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

    /// Aggregate column pattern (minBy, maxBy, sumBy, avgBy, countBy, avgByAs, countDistinct).
    let (|NAggregateColumn|_|) (nexp: NormalizedExpression) =
        match nexp with
        | NMethodCall(m, _) when List.contains m.Method.Name [ nameof minBy; nameof maxBy; nameof sumBy; nameof avgBy; nameof countBy; nameof avgByAs; nameof countDistinct ] ->
            let aggType =
                if m.Method.Name = nameof countDistinct then "COUNTDISTINCT"
                else m.Method.Name.Replace("By", "").Replace("As", "").ToUpper()
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

    /// Renders an aggregate SQL fragment, handling COUNTDISTINCT specially.
    let renderAggregate aggType fqCol =
        if aggType = "COUNTDISTINCT" then $"COUNT(DISTINCT %s{fqCol})"
        else $"%s{aggType}(%s{fqCol})"

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
        | MethodCall m when m.Method.Name.StartsWith("get_Item") ->
            visit m.Object
        | :? System.Linq.Expressions.BlockExpression as block ->
            visit block.Result
        | _ -> notImpl()
    visit exp

/// Converts a SQL function MethodCall expression to a SQL fragment string.
/// Example: LEN(p.FirstName) -> "LEN({p}.{FirstName})"
let rec visitSqlFn (qualifyColumn: string -> MemberInfo -> string) (parameters: ResizeArray<obj>) (exp: Expression) : string =
    match exp with
    | MethodCall m when m.Method.Name = "caseWhen" ->
        let condition = renderExpressionAsSql qualifyColumn parameters m.Arguments.[0]
        let thenValue = renderExpressionAsSql qualifyColumn parameters m.Arguments.[1]
        let elseValue = renderExpressionAsSql qualifyColumn parameters m.Arguments.[2]
        $"CASE WHEN {condition} THEN {thenValue} ELSE {elseValue} END"
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
                | MethodCall mc when mc.Method.Name = "inlineValue" && mc.Arguments.Count = 1 ->
                    let value = compileAndEvaluateExpression mc.Arguments.[0]
                    parameters.Add(value)
                    "?"
                | MethodCall _ as nested ->
                    // Handle nested function calls
                    visitSqlFn qualifyColumn parameters nested
                | _ ->
                    notImplMsg $"Unsupported argument type in SQL function: {arg.NodeType}"
            )
            |> String.concat ", "
        $"{fnName}({args})"
    | _ ->
        notImplMsg $"Expected a method call expression but got: {exp.NodeType}"

/// Renders an expression tree node as a raw SQL fragment (for CASE WHEN conditions etc.)
and renderExpressionAsSql (qualifyColumn: string -> MemberInfo -> string) (parameters: ResizeArray<obj>) (exp: Expression) : string =
    match exp with
    | Member mem ->
        let alias = visitAlias mem.Expression
        qualifyColumn alias mem.Member
    | Constant c when c.Value = null -> "NULL"
    | Constant c when c.Type = typeof<string> -> $"'{c.Value}'"
    | Constant c when c.Type = typeof<bool> -> if c.Value :?> bool then "TRUE" else "FALSE"
    | Constant c -> sprintf "%O" c.Value
    | MethodCall m when m.Method.Name = "inlineValue" && m.Arguments.Count = 1 ->
        let value = compileAndEvaluateExpression m.Arguments.[0]
        parameters.Add(value)
        "?"
    | MethodCall m when List.contains m.Method.Name [ "minBy"; "maxBy"; "sumBy"; "avgBy"; "countBy"; "countDistinct"; "avgByAs" ] ->
        let aggType =
            if m.Method.Name = "countDistinct" then "COUNTDISTINCT"
            else m.Method.Name.Replace("By", "").Replace("As", "").ToUpper()
        match m.Arguments.[0] with
        | Member mem ->
            let alias = visitAlias mem.Expression
            let fqCol = qualifyColumn alias mem.Member
            if aggType = "COUNTDISTINCT" then $"COUNT(DISTINCT {fqCol})"
            else $"{aggType}({fqCol})"
        | _ -> notImplMsg $"Unsupported argument to aggregate in CASE WHEN"
    | MethodCall _ as nested -> visitSqlFn qualifyColumn parameters nested
    | Unary u when u.NodeType = ExpressionType.Convert -> renderExpressionAsSql qualifyColumn parameters u.Operand
    | Binary b ->
        let left = renderExpressionAsSql qualifyColumn parameters b.Left
        let right = renderExpressionAsSql qualifyColumn parameters b.Right
        let op =
            match b.NodeType with
            | ExpressionType.Equal -> "="
            | ExpressionType.NotEqual -> "<>"
            | ExpressionType.GreaterThan -> ">"
            | ExpressionType.GreaterThanOrEqual -> ">="
            | ExpressionType.LessThan -> "<"
            | ExpressionType.LessThanOrEqual -> "<="
            | ExpressionType.Add -> "+"
            | ExpressionType.Subtract -> "-"
            | ExpressionType.Multiply -> "*"
            | ExpressionType.Divide -> "/"
            | ExpressionType.Modulo -> "%"
            | _ -> notImplMsg $"Unsupported CASE WHEN operator: {b.NodeType}"
        $"{left} {op} {right}"
    | _ -> notImplMsg $"Unsupported CASE WHEN expression: {exp.NodeType}"

/// Delegates to existing visitSqlFn by extracting the original MethodCallExpression.
let nVisitSqlFn (qualifyColumn: string -> MemberInfo -> string) (nexp: NormalizedExpression) : string =
    match nexp with
    | NMethodCall(m, _) -> visitSqlFn qualifyColumn (ResizeArray<obj>()) (m :> Expression)
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
                // Try evaluating non-column bool expressions (e.g., capturedOption.IsSome)
                match (try nEvaluate left |> Some with _ -> None) with
                | Some (:? bool as enabled) ->
                    if enabled then visit right (Query()) else query
                | _ ->
                let lt = visit left (Query())
                let rt = visit right (Query())
                query.Where(fun q -> lt).Where(fun q -> rt)

        | NBinaryOr(left, right) ->
            match left with
            // Standard boolean OR: true || right = always true (skip clause), false || right = right determines result
            | NValue enabled ->
                if enabled :?> bool
                then query // true || anything = always true, no filtering needed
                else visit right (Query()) // false || right = right determines result
            | _ ->
                // Try evaluating non-column bool expressions (e.g., capturedOption.IsNone)
                match (try nEvaluate left |> Some with _ -> None) with
                | Some (:? bool as enabled) ->
                    if enabled then query else visit right (Query())
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
                    // Use the first record field for the IS NULL / IS NOT NULL check
                    match FSharp.Reflection.FSharpType.GetRecordFields(innerType) |> Array.tryHead with
                    | None ->
                        notImplMsg $"Anti-join pattern requires a record type with at least one field, but {innerType.Name} has none."
                    | Some firstField ->
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
                query.HavingRaw($"{renderAggregate aggType lt} {comparison} {rt}")
            | NAggregateColumn (aggType, (p, _)), NValue value ->
                // Handle aggregate column to value comparisons
                let alias = visitAlias p.Expression
                let lt = qualifyColumn alias p.Member
                query.HavingRaw($"{renderAggregate aggType lt} {comparison} ?", [value])
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
        | NMethodCall(m, args) when m.Method.Name.StartsWith("get_Item") ->
            visit args.[0]
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
    | SelectedExpression of sqlFragment: string * alias: string option * parameters: obj array
    | SelectedParameter of value: obj * alias: string option


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
    // Map from parameter name to pre-computed selections (for Invoke substitution in anonymous record patterns)
    let paramSubstitutions = System.Collections.Generic.Dictionary<string, Selection list>()
    let rec visit (nexp: NormalizedExpression) : Selection list =
        match nexp with
        | NMethodCall(m, args) when m.Method.Name = "Invoke" ->
            // When invoking a lambda, check if arguments resolve to scalar selections.
            // F# anonymous records in join contexts compile as nested Invoke chains:
            //   (fun fieldName -> New AnonRecord(..., fieldName)).Invoke(o.column)
            // We only substitute parameters whose arguments resolve to scalar selections.
            match m.Object with
            | :? LambdaExpression as lam when lam.Parameters.Count = args.Length ->
                let isScalarType (t: System.Type) =
                    let unwrapped =
                        if t.IsGenericType && (t.GetGenericTypeDefinition() = typedefof<Option<_>> || t.GetGenericTypeDefinition() = typedefof<System.Nullable<_>>) then
                            t.GetGenericArguments().[0]
                        else t
                    unwrapped.IsPrimitive || unwrapped.IsValueType || unwrapped = typeof<string> || unwrapped = typeof<decimal>
                let argResults =
                    [| for i in 0 .. lam.Parameters.Count - 1 do
                        let paramType = lam.Parameters.[i].Type
                        let isScalar = isScalarType paramType
                        let argSels = if isScalar then visit args.[i] else []
                        yield (lam.Parameters.[i].Name, argSels, isScalar) |]
                for (paramName, argSels, isScalar) in argResults do
                    if isScalar then
                        paramSubstitutions.[paramName] <- argSels
                let result = visit (ExpressionNormalizer.toNormalizedExpression (lam.Body :> Expression))
                for (paramName, _, isScalar) in argResults do
                    if isScalar then
                        paramSubstitutions.Remove(paramName) |> ignore
                result
            | _ ->
                visit args.[0]
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
                let parms = ResizeArray<obj>()
                let sqlFragment = visitSqlFn qualifyCol parms (m :> Expression)
                [ SelectedExpression (sqlFragment, None, parms.ToArray()) ]
        | NAggregateColumn (aggType, (p, _)) ->
            let alias = visitAlias p.Expression
            let fqCol = $"{{%s{alias}}}.{{%s{p.Member.Name}}}"
            [ SelectedExpression (renderAggregate aggType fqCol, None, [||]) ]
        | NMethodCall(m, args) when m.Method.Name = "inlineValue" && args.Length = 1 ->
            let value = compileAndEvaluateExpression m.Arguments.[0]
            [ SelectedParameter (value, None) ]
        | NMethodCall(m, _) ->
            // Treat any other method call as a SQL function
            let qualifyCol alias (mem: MemberInfo) = $"{{%s{alias}}}.{{%s{mem.Name}}}"
            let parms = ResizeArray<obj>()
            let sqlFragment = visitSqlFn qualifyCol parms (m :> Expression)
            [ SelectedExpression (sqlFragment, None, parms.ToArray()) ]
        | NNew(n, args) ->
            // Detect whether this is a tuple type (System.Tuple, System.ValueTuple) which should NOT get aliases
            let isTupleType =
                let t = n.Type
                t.Namespace = "System" && (t.Name.StartsWith("Tuple") || t.Name.StartsWith("ValueTuple"))
            // Get member names: from Members (C# anonymous types) or constructor parameters (F# anonymous records)
            let memberNames =
                if isTupleType then
                    Array.create args.Length None
                elif n.Members <> null && n.Members.Count = args.Length then
                    n.Members |> Seq.map (fun m -> Some m.Name) |> Seq.toArray
                else
                    let ctorParams = n.Constructor.GetParameters()
                    if ctorParams.Length = args.Length && ctorParams.Length > 0 then
                        ctorParams |> Array.map (fun p -> Some p.Name)
                    else
                        Array.create args.Length None
            if memberNames |> Array.exists Option.isSome then
                // Named constructor (anonymous record or named type) — attach field names as aliases
                Seq.zip args memberNames
                |> Seq.map (fun (arg, nameOpt) ->
                    let selections = visit arg
                    match nameOpt with
                    | None -> selections
                    | Some name ->
                        selections |> List.map (fun sel ->
                            match sel with
                            | SelectedExpression (frag, _, parms) ->
                                SelectedExpression ($"{frag} AS \"{name}\"", Some name, parms)
                            | SelectedParameter (v, _) ->
                                SelectedParameter (v, Some name)
                            | SelectedColumn (tblAlias, col, colType, isOpt, isNullable) when col <> name ->
                                SelectedExpression ($"\"{tblAlias}\".\"{col}\" AS \"{name}\"", Some name, [||])
                            | other -> other
                        )
                )
                |> Seq.toList |> List.concat
            else
                args |> List.collect visit
        | NParameter p ->
            match paramSubstitutions.TryGetValue(p.Name) with
            | true, selections -> selections
            | _ -> [ SelectedTable (p.Name, p.Type) ]
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

/// Tracks how each alias is used in the projection expression.
type AliasUsage = {
    mutable RequiresFullRecord: bool
    UsedColumns: System.Collections.Generic.HashSet<string>
}

/// Analyzes a projection expression to determine, per alias, whether the full record is needed
/// or only specific columns are accessed.
let analyzeProjectionShape
    (body: Expression)
    (outerParamNames: System.Collections.Generic.HashSet<string>)
    (outerParamTypes: System.Collections.Generic.Dictionary<string, Type>)
    (typeToAlias: System.Collections.Generic.Dictionary<Type, string>)
    =
    let usageMap = System.Collections.Generic.Dictionary<string, AliasUsage>()

    let getOrCreate (alias: string) =
        match usageMap.TryGetValue(alias) with
        | true, u -> u
        | false, _ ->
            let u = { RequiresFullRecord = false; UsedColumns = System.Collections.Generic.HashSet<string>() }
            usageMap.[alias] <- u
            u

    let isGeneratedRecordType (t: Type) =
        t <> null && t.DeclaringType <> null && FSharp.Reflection.FSharpType.IsRecord(t, System.Reflection.BindingFlags.Public ||| System.Reflection.BindingFlags.NonPublic)

    let isOptionOfGeneratedRecordType (t: Type) =
        t <> null && isOptionType t && isGeneratedRecordType (t.GetGenericArguments().[0])

    let isTableType (t: Type) =
        isGeneratedRecordType t || isOptionOfGeneratedRecordType t

    /// Resolve alias from a parameter expression using the same logic as the rewriter.
    let resolveAlias (p: ParameterExpression) =
        if outerParamNames.Contains(p.Name) then p.Name
        else
            match typeToAlias.TryGetValue(p.Type) with
            | true, alias -> alias
            | false, _ -> p.Name

    /// Try to get the alias from an expression that bottoms out at a parameter.
    let rec tryGetAlias (e: Expression) =
        match e with
        | Parameter p when isTableType p.Type -> Some (resolveAlias p)
        | Member m when m.Member.DeclaringType <> null && isOptionOrNullableType m.Member.DeclaringType ->
            tryGetAlias m.Expression
        | _ -> None

    /// Check if a MethodCall is an Option.map/Option.bind with a simple field accessor lambda.
    /// Returns Some(sourceExpr, fieldName) if so.
    let (|OptionMapField|_|) (exp: Expression) =
        match exp with
        | MethodCall m when m.Method.Name = "op_PipeRight" && m.Arguments.Count = 2 ->
            let source = m.Arguments.[0]
            let rec containsOptionMap (e: Expression) =
                match e with
                | :? MethodCallExpression as mc ->
                    (mc.Method.Name = "Map" && mc.Method.DeclaringType <> null && mc.Method.DeclaringType.Name = "OptionModule")
                    || mc.Arguments |> Seq.exists containsOptionMap
                    || (mc.Object <> null && containsOptionMap mc.Object)
                | :? LambdaExpression as lam -> containsOptionMap lam.Body
                | _ -> false
            if containsOptionMap m.Arguments.[1] then
                let rec findMapLambda (e: Expression) =
                    match e with
                    | :? MethodCallExpression as invoke when invoke.Method.Name = "Invoke" ->
                        match invoke.Arguments.[0] with
                        | :? MethodCallExpression as toFF when toFF.Method.Name = "ToFSharpFunc" ->
                            match toFF.Arguments.[0] with
                            | :? LambdaExpression as mapLam -> Some mapLam
                            | _ -> None
                        | _ -> None
                    | _ -> None
                match findMapLambda m.Arguments.[1] with
                | Some mapLam ->
                    match mapLam.Body with
                    | Member memberExp -> Some (source, memberExp.Member.Name)
                    | _ -> None
                | None -> None
            else None
        | _ -> None

    let rec analyze (exp: Expression) =
        match exp with
        // Option.map _.Field — only needs the specific column
        | OptionMapField (source, fieldName) ->
            match tryGetAlias source with
            | Some alias -> (getOrCreate alias).UsedColumns.Add(fieldName) |> ignore
            | None -> analyzeChildren exp

        // Match/switch scrutinee — marks full record required
        | :? SwitchExpression as sw ->
            match tryGetAlias sw.SwitchValue with
            | Some alias -> (getOrCreate alias).RequiresFullRecord <- true
            | None -> ()
            analyzeChildren exp

        // Conditional — check if test is on an alias (match on option compiles to conditional)
        | :? ConditionalExpression as c ->
            // For `match optAlias with Some x -> ... | None -> ...`, the test is typically
            // a check like `optAlias.get_Tag() == 1` or similar. We detect if the test references a table alias.
            analyzeConditionalTest c.Test
            analyze c.IfTrue
            analyze c.IfFalse

        // Direct member access on a table parameter: alias.Field
        | Member m when m.Expression <> null ->
            match tryGetAlias m.Expression with
            | Some alias when not (isOptionOrNullableType m.Member.DeclaringType) ->
                (getOrCreate alias).UsedColumns.Add(m.Member.Name) |> ignore
            | _ -> analyzeChildren exp

        // A standalone table-typed parameter (not accessed as .Field or via Option.map)
        | Parameter p when isTableType p.Type ->
            let alias = resolveAlias p
            (getOrCreate alias).RequiresFullRecord <- true

        | _ -> analyzeChildren exp

    and analyzeChildren (exp: Expression) =
        match exp with
        | Lambda x -> analyze x.Body
        | MethodCall m ->
            if m.Object <> null then analyze m.Object
            for arg in m.Arguments do analyze arg
        | :? InvocationExpression as inv ->
            analyze inv.Expression
            for arg in inv.Arguments do analyze arg
        | New n ->
            for arg in n.Arguments do analyze arg
        | Unary u -> analyze u.Operand
        | Binary b -> analyze b.Left; analyze b.Right
        | :? ConditionalExpression as c ->
            analyze c.Test; analyze c.IfTrue; analyze c.IfFalse
        | :? BlockExpression as blk ->
            for e in blk.Expressions do analyze e
        | :? SwitchExpression as sw ->
            analyze sw.SwitchValue
            if sw.DefaultBody <> null then analyze sw.DefaultBody
            for case in sw.Cases do
                for tv in case.TestValues do analyze tv
                analyze case.Body
        | _ -> ()

    and analyzeConditionalTest (exp: Expression) =
        // Walk the test expression looking for table-typed parameter references.
        // If a table-typed parameter is used in the test of a conditional (match expression),
        // it requires the full record.
        match exp with
        | MethodCall m ->
            // Check if any argument is a table-typed parameter access
            let mutable found = false
            if m.Object <> null then
                match tryGetAlias m.Object with
                | Some alias -> (getOrCreate alias).RequiresFullRecord <- true; found <- true
                | None -> ()
            for arg in m.Arguments do
                match tryGetAlias arg with
                | Some alias -> (getOrCreate alias).RequiresFullRecord <- true; found <- true
                | None -> ()
            if not found then
                if m.Object <> null then analyzeConditionalTest m.Object
                for arg in m.Arguments do analyzeConditionalTest arg
        | Member m when m.Expression <> null ->
            match tryGetAlias m.Expression with
            | Some alias when isOptionOrNullableType m.Member.DeclaringType || m.Member.Name = "get_Tag" || m.Member.Name = "Tag" ->
                (getOrCreate alias).RequiresFullRecord <- true
            | _ -> analyzeConditionalTest m.Expression
        | Unary u -> analyzeConditionalTest u.Operand
        | Binary b -> analyzeConditionalTest b.Left; analyzeConditionalTest b.Right
        | _ -> ()

    analyze body
    usageMap

/// Visits a selectExpr expression in two passes:
/// Pass 1: Walk the expression tree to identify database leaf sub-expressions and rewrite the tree.
/// Pass 2 (at runtime): Use the compiled mapper with leaf values to produce the final result.
let visitSelectExpr<'T, 'Selected> (selectExpression: Expression<Func<'T, 'Selected>>) =
    let leaves = ResizeArray<ExprLeaf>()
    let leafIndex = ref 0
    // Deduplication: (tableAlias.column) -> index
    let leafKeys = System.Collections.Generic.Dictionary<string, int>()
    let sqlExprCounter = ref 0

    let paramArray = Expression.Parameter(typeof<obj[]>, "leafValues")

    let getOrAddLeaf (key: string) (mkLeaf: int -> ExprLeaf) (leafType: Type) =
        match leafKeys.TryGetValue(key) with
        | true, existingIdx ->
            // Return array access for existing leaf
            Expression.Convert(
                Expression.ArrayIndex(paramArray, Expression.Constant(existingIdx)),
                leafType) :> Expression
        | false, _ ->
            let idx = !leafIndex
            leafIndex := idx + 1
            let leaf = mkLeaf idx
            leaves.Add(leaf)
            leafKeys.[key] <- idx
            Expression.Convert(
                Expression.ArrayIndex(paramArray, Expression.Constant(idx)),
                leafType) :> Expression

    /// Checks if a type is a generated record type (has a declaring type, indicating it's a nested type under a schema module).
    let isGeneratedRecordType (t: Type) =
        t <> null && t.DeclaringType <> null && FSharp.Reflection.FSharpType.IsRecord(t, System.Reflection.BindingFlags.Public ||| System.Reflection.BindingFlags.NonPublic)

    /// Checks if a type is Option<RecordType> where RecordType is a generated record type.
    let isOptionOfGeneratedRecordType (t: Type) =
        t <> null && isOptionType t && isGeneratedRecordType (t.GetGenericArguments().[0])

    /// Checks if a type is a generated record type or an Option wrapping one.
    let isTableType (t: Type) =
        isGeneratedRecordType t || isOptionOfGeneratedRecordType t

    /// Checks if a method belongs to the SqlHydra.Query assembly (i.e., is a SQL function).
    let isSqlHydraMethod (m: MethodInfo) =
        m.Module.Name = "SqlHydra.Query.dll"

    // --- Alias resolution dictionaries ---
    // Primary: maps inner lambda ParameterExpressions to their resolved outer alias
    let paramToAlias = System.Collections.Generic.Dictionary<ParameterExpression, string>()
    // Fallback: maps types (both Option<T> and T) to alias. Known limitation: if two outer
    // params share the same inner type, the last one wins.
    let typeToAlias = System.Collections.Generic.Dictionary<Type, string>()
    // Names of outer parameters (the table aliases like p, o, sr, r)
    let outerParamNames = System.Collections.Generic.HashSet<string>()
    // Maps outer param alias to its declared type (for determining optionality)
    let outerParamTypes = System.Collections.Generic.Dictionary<string, Type>()
    // Projection shape analysis result (populated after unwrapBodyWithParams)
    let mutable aliasUsage = System.Collections.Generic.Dictionary<string, AliasUsage>()

    /// Unwrap the Lambda -> Invoke -> Lambda nesting that F# CEs generate,
    /// collecting outer parameters along the way.
    let rec unwrapBodyWithParams (exp: Expression) =
        match exp with
        | Lambda x ->
            // Collect outer lambda parameters (these are the table aliases)
            for p in x.Parameters do
                outerParamNames.Add(p.Name) |> ignore
                outerParamTypes.[p.Name] <- p.Type
                // Register type mappings for fallback resolution
                if isTableType p.Type then
                    typeToAlias.[p.Type] <- p.Name
                    if isOptionType p.Type then
                        typeToAlias.[p.Type.GetGenericArguments().[0]] <- p.Name
            unwrapBodyWithParams x.Body
        | MethodCall m when m.Method.Name = "Invoke" -> unwrapBodyWithParams m.Object
        | _ -> exp

    /// Resolve the table alias for a ParameterExpression.
    let resolveTableAlias (p: ParameterExpression) =
        if outerParamNames.Contains(p.Name) then p.Name
        else
            match paramToAlias.TryGetValue(p) with
            | true, alias -> alias
            | false, _ ->
                // Fallback: look up by type
                match typeToAlias.TryGetValue(p.Type) with
                | true, alias -> alias
                | false, _ -> p.Name

    /// Check if an expression resolves to a table-typed parameter.
    let rec isTableExpr (e: Expression) =
        match e with
        | Parameter p -> isTableType p.Type
        | Member inner when inner.Member.DeclaringType <> null && inner.Member.DeclaringType |> isOptionOrNullableType ->
            isTableExpr inner.Expression
        | _ -> false

    /// Resolve alias from an arbitrary expression that bottoms out at a parameter.
    let rec resolveAliasFromExpr (e: Expression) =
        match e with
        | Parameter p -> resolveTableAlias p
        | Member inner when inner.Member.DeclaringType <> null && inner.Member.DeclaringType |> isOptionOrNullableType ->
            resolveAliasFromExpr inner.Expression
        | _ -> visitAlias e

    /// Returns the table alias if the expression has provenance from a CE table parameter.
    let rec getProvenance (e: Expression) : string option =
        match e with
        | Parameter p ->
            if outerParamNames.Contains(p.Name) then Some p.Name
            else
                match paramToAlias.TryGetValue(p) with
                | true, alias -> Some alias
                | false, _ -> None
        | Member m when m.Member.DeclaringType <> null && isOptionOrNullableType m.Member.DeclaringType ->
            getProvenance m.Expression
        | _ -> None

    let rec rewrite (exp: Expression) : Expression =
        match exp with
        // --- SQL leaf detection (before generic cases) ---
        | AggregateColumn (aggType, (p, _)) ->
            let alias = visitAlias p.Expression
            let fqCol = $"[{alias}].[{p.Member.Name}]" // NOTE: SqlKata will translate [ ] to proper quoting for the target dialect.
            let sqlFragment = renderAggregate aggType fqCol
            let exprAlias = $"__hydra_expr_{!sqlExprCounter}"
            sqlExprCounter := !sqlExprCounter + 1
            let key = $"__sqlfn:{sqlFragment}"
            getOrAddLeaf key
                (fun idx -> SqlExprLeaf (sqlFragment, exp.Type, exprAlias, idx))
                exp.Type

        | MethodCall m when isSqlHydraMethod m.Method ->
            rewriteSqlFunction m exp

        // --- Option.map _.Field pattern (column-only when analysis says so) ---
        | MethodCall m when m.Method.Name = "op_PipeRight" && m.Arguments.Count = 2 ->
            let source = m.Arguments.[0]
            let rec containsOptionMap (e: Expression) =
                match e with
                | :? MethodCallExpression as mc ->
                    (mc.Method.Name = "Map" && mc.Method.DeclaringType <> null && mc.Method.DeclaringType.Name = "OptionModule")
                    || mc.Arguments |> Seq.exists containsOptionMap
                    || (mc.Object <> null && containsOptionMap mc.Object)
                | :? LambdaExpression as lam -> containsOptionMap lam.Body
                | _ -> false
            if containsOptionMap m.Arguments.[1] then
                // Try to extract the field accessor lambda
                let rec findMapLambda (e: Expression) =
                    match e with
                    | :? MethodCallExpression as invoke when invoke.Method.Name = "Invoke" ->
                        match invoke.Arguments.[0] with
                        | :? MethodCallExpression as toFF when toFF.Method.Name = "ToFSharpFunc" ->
                            match toFF.Arguments.[0] with
                            | :? LambdaExpression as mapLam -> Some mapLam
                            | _ -> None
                        | _ -> None
                    | _ -> None
                // Resolve source alias
                let sourceAlias =
                    let rec tryResolve (e: Expression) =
                        match e with
                        | Parameter p -> Some (resolveTableAlias p)
                        | Member inner when inner.Member.DeclaringType <> null && isOptionOrNullableType inner.Member.DeclaringType ->
                            tryResolve inner.Expression
                        | _ -> None
                    tryResolve source
                match sourceAlias, findMapLambda m.Arguments.[1] with
                | Some alias, Some mapLam when
                    aliasUsage.ContainsKey(alias) && not aliasUsage.[alias].RequiresFullRecord ->
                    match mapLam.Body with
                    | Member memberExp ->
                        // Column-only: register a ColumnLeaf with isOpt=true (comes through Option.map)
                        let isOuterOptional =
                            match outerParamTypes.TryGetValue(alias) with
                            | true, outerType -> isOptionType outerType
                            | false, _ -> false
                        let colType =
                            if isOuterOptional then
                                typedefof<Option<_>>.MakeGenericType(memberExp.Type)
                            else
                                memberExp.Type
                        let key = $"{alias}.{memberExp.Member.Name}"
                        getOrAddLeaf key
                            (fun idx -> ColumnLeaf (alias, memberExp.Member.Name, colType, true, false, idx))
                            colType
                    | _ ->
                        // Fall through: rewrite children
                        let newArgs = m.Arguments |> Seq.map rewrite |> Seq.toArray
                        let newObj = if m.Object <> null then rewrite m.Object else null
                        let argsChanged = Seq.zip m.Arguments newArgs |> Seq.exists (fun (a, b) -> not (obj.ReferenceEquals(a, b)))
                        let objChanged = m.Object <> null && not (obj.ReferenceEquals(newObj, m.Object))
                        if argsChanged || objChanged then
                            if m.Object <> null then Expression.Call(newObj, m.Method, newArgs) :> Expression
                            else Expression.Call(m.Method, newArgs) :> Expression
                        else exp
                | _ ->
                    // Full record needed or can't resolve: fall through to generic MethodCall rewrite
                    let newArgs = m.Arguments |> Seq.map rewrite |> Seq.toArray
                    let newObj = if m.Object <> null then rewrite m.Object else null
                    let argsChanged = Seq.zip m.Arguments newArgs |> Seq.exists (fun (a, b) -> not (obj.ReferenceEquals(a, b)))
                    let objChanged = m.Object <> null && not (obj.ReferenceEquals(newObj, m.Object))
                    if argsChanged || objChanged then
                        if m.Object <> null then Expression.Call(newObj, m.Method, newArgs) :> Expression
                        else Expression.Call(m.Method, newArgs) :> Expression
                    else exp
            else
                // Not an Option.map pipe: generic MethodCall rewrite
                let newArgs = m.Arguments |> Seq.map rewrite |> Seq.toArray
                let newObj = if m.Object <> null then rewrite m.Object else null
                let argsChanged = Seq.zip m.Arguments newArgs |> Seq.exists (fun (a, b) -> not (obj.ReferenceEquals(a, b)))
                let objChanged = m.Object <> null && not (obj.ReferenceEquals(newObj, m.Object))
                if argsChanged || objChanged then
                    if m.Object <> null then Expression.Call(newObj, m.Method, newArgs) :> Expression
                    else Expression.Call(m.Method, newArgs) :> Expression
                else exp

        // --- Generic recursive cases ---
        | Lambda x ->
            // If any lambda parameter has a table type, pre-populate paramToAlias
            for p in x.Parameters do
                if isTableType p.Type then
                    match typeToAlias.TryGetValue(p.Type) with
                    | true, alias -> paramToAlias.[p] <- alias
                    | false, _ -> ()
            let newBody = rewrite x.Body
            if obj.ReferenceEquals(newBody, x.Body) then exp
            else Expression.Lambda(x.Type, newBody, x.Parameters) :> Expression

        | MethodCall m ->
            let newArgs = m.Arguments |> Seq.map rewrite |> Seq.toArray
            let newObj = if m.Object <> null then rewrite m.Object else null
            let argsChanged = Seq.zip m.Arguments newArgs |> Seq.exists (fun (a, b) -> not (obj.ReferenceEquals(a, b)))
            let objChanged = m.Object <> null && not (obj.ReferenceEquals(newObj, m.Object))
            if argsChanged || objChanged then
                if m.Object <> null then
                    Expression.Call(newObj, m.Method, newArgs) :> Expression
                else
                    Expression.Call(m.Method, newArgs) :> Expression
            else exp

        | :? InvocationExpression as inv ->
            // Propagate provenance: if this is Invoke(Lambda(params), args),
            // set provenance for lambda params from the invocation arguments.
            match inv.Expression with
            | :? LambdaExpression as lam ->
                for i in 0 .. min (lam.Parameters.Count - 1) (inv.Arguments.Count - 1) do
                    let param = lam.Parameters.[i]
                    match getProvenance inv.Arguments.[i] with
                    | Some alias -> paramToAlias.[param] <- alias
                    | None -> ()
            | _ -> ()
            let newExpr = rewrite inv.Expression
            let newArgs = inv.Arguments |> Seq.map rewrite |> Seq.toArray
            let exprChanged = not (obj.ReferenceEquals(newExpr, inv.Expression))
            let argsChanged = Seq.zip inv.Arguments newArgs |> Seq.exists (fun (a, b) -> not (obj.ReferenceEquals(a, b)))
            if exprChanged || argsChanged then
                Expression.Invoke(newExpr, newArgs) :> Expression
            else exp

        | New n ->
            let newArgs = n.Arguments |> Seq.map rewrite |> Seq.toArray
            Expression.New(n.Constructor, newArgs) :> Expression

        | :? NewArrayExpression as na when na.NodeType = ExpressionType.NewArrayInit ->
            let newExprs = na.Expressions |> Seq.map rewrite |> Seq.toArray
            Expression.NewArrayInit(na.Type.GetElementType(), newExprs) :> Expression

        | :? MemberInitExpression as mi ->
            let newExpr = rewrite (mi.NewExpression :> Expression) :?> NewExpression
            Expression.MemberInit(newExpr, mi.Bindings) :> Expression

        | :? ListInitExpression as li ->
            let newExpr = rewrite (li.NewExpression :> Expression) :?> NewExpression
            Expression.ListInit(newExpr, li.Initializers) :> Expression

        | :? BlockExpression as blk ->
            // Propagate provenance for block variables assigned from provenance-bearing expressions
            for expr in blk.Expressions do
                if expr.NodeType = ExpressionType.Assign then
                    let bin = expr :?> BinaryExpression
                    match bin.Left with
                    | Parameter p ->
                        match getProvenance bin.Right with
                        | Some alias -> paramToAlias.[p] <- alias
                        | None -> ()
                    | _ -> ()
            let newExprs = blk.Expressions |> Seq.map rewrite |> Seq.toArray
            Expression.Block(blk.Type, blk.Variables, newExprs) :> Expression

        | :? LoopExpression as lp ->
            let newBody = rewrite lp.Body
            if obj.ReferenceEquals(newBody, lp.Body) then exp
            else Expression.Loop(newBody, lp.BreakLabel, lp.ContinueLabel) :> Expression

        | :? TryExpression as tr ->
            let newBody = rewrite tr.Body
            let newFault = if tr.Fault <> null then rewrite tr.Fault else null
            let newFinally = if tr.Finally <> null then rewrite tr.Finally else null
            Expression.MakeTry(tr.Type, newBody, newFinally, newFault, tr.Handlers) :> Expression

        | :? SwitchExpression as sw ->
            let newVal = rewrite sw.SwitchValue
            let newDefault = if sw.DefaultBody <> null then rewrite sw.DefaultBody else null
            Expression.Switch(sw.Type, newVal, newDefault, sw.Comparison, sw.Cases) :> Expression

        // --- Unary (incl. Quote, TypeAs, Convert) ---
        | Unary u ->
            let newOperand = rewrite u.Operand
            if obj.ReferenceEquals(newOperand, u.Operand) then exp
            else Expression.MakeUnary(u.NodeType, newOperand, u.Type, u.Method) :> Expression

        | Binary b ->
            let newLeft = rewrite b.Left
            let newRight = rewrite b.Right
            if obj.ReferenceEquals(newLeft, b.Left) && obj.ReferenceEquals(newRight, b.Right) then exp
            else Expression.MakeBinary(b.NodeType, newLeft, newRight, b.IsLiftedToNull, b.Method) :> Expression

        | :? ConditionalExpression as c ->
            let newTest = rewrite c.Test
            let newIfTrue = rewrite c.IfTrue
            let newIfFalse = rewrite c.IfFalse
            if obj.ReferenceEquals(newTest, c.Test) && obj.ReferenceEquals(newIfTrue, c.IfTrue) && obj.ReferenceEquals(newIfFalse, c.IfFalse) then exp
            else Expression.Condition(newTest, newIfTrue, newIfFalse) :> Expression

        | :? TypeBinaryExpression as tb ->
            let newExpr = rewrite tb.Expression
            if obj.ReferenceEquals(newExpr, tb.Expression) then exp
            else
                if tb.NodeType = ExpressionType.TypeIs then Expression.TypeIs(newExpr, tb.TypeOperand) :> Expression
                else Expression.TypeEqual(newExpr, tb.TypeOperand) :> Expression

        // --- Leaf detection ---
        | Parameter p when isTableType p.Type ->
            let alias = resolveTableAlias p
            // Only create a TableLeaf if the analysis determined the full record is needed
            let needsFullRecord =
                match aliasUsage.TryGetValue(alias) with
                | true, usage -> usage.RequiresFullRecord
                | false, _ -> true // Default to full record if not analyzed
            if needsFullRecord then
                let key = $"__table:{alias}"
                getOrAddLeaf key
                    (fun idx -> TableLeaf (alias, p.Type, idx))
                    p.Type
            else
                // Column-only alias: individual columns will be handled by member access cases
                // Return the expression unchanged (it will be pruned by outer rewrite)
                exp

        | Parameter _ -> exp

        | Member m when m.Member.DeclaringType <> null && m.Member.DeclaringType |> isOptionOrNullableType ->
            let newExpr = rewrite m.Expression
            if obj.ReferenceEquals(newExpr, m.Expression) then exp
            else Expression.MakeMemberAccess(newExpr, m.Member) :> Expression

        | Member m when m.Expression <> null && isTableExpr m.Expression ->
            // Column access on a table parameter
            let alias = resolveAliasFromExpr m.Expression
            let tableKey = $"__table:{alias}"
            match leafKeys.TryGetValue(tableKey) with
            | true, tableIdx ->
                let tableAccess =
                    Expression.Convert(
                        Expression.ArrayIndex(paramArray, Expression.Constant(tableIdx)),
                        leaves.[tableIdx] |> function TableLeaf (_, t, _) -> t | _ -> failwith "expected TableLeaf") :> Expression
                let recordAccess =
                    let tableType = (leaves.[tableIdx] |> function TableLeaf (_, t, _) -> t | _ -> failwith "expected TableLeaf")
                    if isOptionType tableType then
                        let valueProp = tableType.GetProperty("Value")
                        Expression.MakeMemberAccess(tableAccess, valueProp) :> Expression
                    else
                        tableAccess
                Expression.MakeMemberAccess(recordAccess, m.Member) :> Expression
            | false, _ ->
                // Determine optionality: column is optional if it's already Option/Nullable,
                // or if the outer param it resolves to is Option<RecordType> (leftJoin)
                let isOptional =
                    m.Type.IsGenericType && m.Type.GetGenericTypeDefinition() = typedefof<Option<_>>
                let isNullable =
                    m.Type.IsGenericType && m.Type.GetGenericTypeDefinition() = typedefof<Nullable<_>>
                // Check if this column comes through an optional outer table
                let isOuterOptional =
                    match outerParamTypes.TryGetValue(alias) with
                    | true, outerType -> isOptionType outerType
                    | false, _ -> false
                let finalOptional = isOptional || isOuterOptional
                let colType =
                    if isOuterOptional && not isOptional && not isNullable then
                        // Wrap column type in Option since the outer table is optional
                        typedefof<Option<_>>.MakeGenericType(m.Type)
                    else
                        m.Type
                let key = $"{alias}.{m.Member.Name}"
                getOrAddLeaf key
                    (fun idx -> ColumnLeaf (alias, m.Member.Name, colType, finalOptional, isNullable, idx))
                    colType

        | Member m when m.Expression <> null ->
            let newExpr = rewrite m.Expression
            if obj.ReferenceEquals(newExpr, m.Expression) then exp
            else Expression.MakeMemberAccess(newExpr, m.Member) :> Expression

        | Constant _ -> exp

        | :? DefaultExpression -> exp

        | _ -> exp

    and rewriteSqlFunction (m: MethodCallExpression) (originalExp: Expression) =
        let qualifyCol alias (mem: MemberInfo) = $"[{alias}].[{mem.Name}]"
        /// Like visitSqlFn but uses provenance-aware alias resolution
        /// so that pattern variables (e.g., from match) resolve to
        /// the correct table alias.
        let rec visitSqlFnWithProvenance (exp: Expression) : string =
            match exp with
            | MethodCall m when m.Method.Name = "caseWhen" ->
                let condition = renderExprAsSqlProv m.Arguments.[0]
                let thenValue = renderExprAsSqlProv m.Arguments.[1]
                let elseValue = renderExprAsSqlProv m.Arguments.[2]
                $"CASE WHEN {condition} THEN {thenValue} ELSE {elseValue} END"
            | MethodCall m ->
                let fnName = m.Method.Name
                let args =
                    m.Arguments
                    |> Seq.map (fun arg ->
                        match arg with
                        | Member mem ->
                            let alias = resolveAliasFromExpr mem.Expression
                            qualifyCol alias mem.Member
                        | Constant c when c.Value = null ->
                            "NULL"
                        | Constant c when c.Type = typeof<string> ->
                            $"'{c.Value}'"
                        | Constant c ->
                            sprintf "%O" c.Value
                        | MethodCall _ as nested ->
                            visitSqlFnWithProvenance nested
                        | _ ->
                            notImplMsg $"Unsupported argument type in SQL function: {arg.NodeType}"
                    )
                    |> String.concat ", "
                $"{fnName}({args})"
            | _ ->
                notImplMsg $"Expected a method call expression but got: {exp.NodeType}"

        and renderExprAsSqlProv (exp: Expression) : string =
            match exp with
            | Member mem ->
                let alias = resolveAliasFromExpr mem.Expression
                qualifyCol alias mem.Member
            | Constant c when c.Value = null -> "NULL"
            | Constant c when c.Type = typeof<string> -> $"'{c.Value}'"
            | Constant c when c.Type = typeof<bool> -> if c.Value :?> bool then "TRUE" else "FALSE"
            | Constant c -> sprintf "%O" c.Value
            | MethodCall m when List.contains m.Method.Name [ "minBy"; "maxBy"; "sumBy"; "avgBy"; "countBy"; "countDistinct"; "avgByAs" ] ->
                let aggType =
                    if m.Method.Name = "countDistinct" then "COUNTDISTINCT"
                    else m.Method.Name.Replace("By", "").Replace("As", "").ToUpper()
                match m.Arguments.[0] with
                | Member mem ->
                    let alias = resolveAliasFromExpr mem.Expression
                    let fqCol = qualifyCol alias mem.Member
                    if aggType = "COUNTDISTINCT" then $"COUNT(DISTINCT {fqCol})"
                    else $"{aggType}({fqCol})"
                | _ -> notImplMsg $"Unsupported argument to aggregate in CASE WHEN"
            | MethodCall _ as nested -> visitSqlFnWithProvenance nested
            | Unary u when u.NodeType = ExpressionType.Convert -> renderExprAsSqlProv u.Operand
            | Binary b ->
                let left = renderExprAsSqlProv b.Left
                let right = renderExprAsSqlProv b.Right
                let op =
                    match b.NodeType with
                    | ExpressionType.Equal -> "="
                    | ExpressionType.NotEqual -> "<>"
                    | ExpressionType.GreaterThan -> ">"
                    | ExpressionType.GreaterThanOrEqual -> ">="
                    | ExpressionType.LessThan -> "<"
                    | ExpressionType.LessThanOrEqual -> "<="
                    | ExpressionType.Add -> "+"
                    | ExpressionType.Subtract -> "-"
                    | ExpressionType.Multiply -> "*"
                    | ExpressionType.Divide -> "/"
                    | ExpressionType.Modulo -> "%"
                    | _ -> notImplMsg $"Unsupported CASE WHEN operator: {b.NodeType}"
                $"{left} {op} {right}"
            | _ -> notImplMsg $"Unsupported CASE WHEN expression: {exp.NodeType}"

        let sqlFragment = visitSqlFnWithProvenance (m :> Expression)
        let exprAlias = $"__hydra_expr_{!sqlExprCounter}"
        sqlExprCounter := !sqlExprCounter + 1
        let key = $"__sqlfn:{sqlFragment}"
        getOrAddLeaf key
            (fun idx -> SqlExprLeaf (sqlFragment, originalExp.Type, exprAlias, idx))
            originalExp.Type

    let actualBody = unwrapBodyWithParams (selectExpression :> Expression)

    // Run projection shape analysis before rewriting
    aliasUsage <- analyzeProjectionShape actualBody outerParamNames outerParamTypes typeToAlias

    let rewrittenBody = rewrite actualBody

    // Build the leaf tuple type and compiled mapper
    let leafTypes =
        leaves
        |> Seq.map (fun leaf ->
            match leaf with
            | TableLeaf (_, tableType, _) -> tableType
            | ColumnLeaf (_, _, colType, _, _, _) -> colType
            | SqlExprLeaf (_, resultType, _, _) -> resultType)
        |> Seq.toArray

    let leafTupleType =
        if leafTypes.Length = 0 then typeof<unit>
        elif leafTypes.Length = 1 then leafTypes.[0]
        else FSharp.Reflection.FSharpType.MakeTupleType(leafTypes)

    // Compile the mapper: obj[] -> obj
    let convertedBody =
        if rewrittenBody.Type.IsValueType then
            Expression.Convert(rewrittenBody, typeof<obj>) :> Expression
        else
            rewrittenBody
    let mapperLambda = Expression.Lambda<Func<obj[], obj>>(convertedBody, paramArray)
    let compiledMapper =
        try mapperLambda.CompileFast<Func<obj[], obj>>()
        with _ -> mapperLambda.Compile()

    {
        Leaves = leaves |> Seq.toList
        LeafTupleType = leafTupleType
        CompiledMapper = compiledMapper
    }
