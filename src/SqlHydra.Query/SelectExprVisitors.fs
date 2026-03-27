/// Experimental two-pass select expression visitor.
/// Analyzes projection expressions to extract database leaf sub-expressions,
/// then rewrites the expression tree for runtime mapping.
module internal SqlHydra.Query.SelectExprVisitors

open System
open System.Linq.Expressions
open System.Reflection
open SqlKata
open FastExpressionCompiler
open LinqExpressionVisitors

/// A database-sourced leaf in a selectExpr expression tree.
type ExprLeaf =
    | TableLeaf of tableAlias: string * tableType: Type * index: int
    | ColumnLeaf of tableAlias: string * column: string * columnType: Type
                   * isOpt: bool * isNullable: bool * index: int
    | SqlExprLeaf of sqlFragment: string * resultType: Type * alias: string * index: int

/// Result of visiting a selectExpr expression (Pass 1).
type SelectExprInfo = {
    Leaves: ExprLeaf list
    LeafTupleType: Type
    CompiledMapper: Func<obj[], obj>
}

module SelectExprStore =
    open System.Runtime.CompilerServices

    let private store = ConditionalWeakTable<SqlKata.Query, SelectExprInfo>()

    let set (query: SqlKata.Query) (info: SelectExprInfo) =
        store.Remove(query) |> ignore
        store.Add(query, info)

    let tryGet (query: SqlKata.Query) =
        match store.TryGetValue(query) with
        | true, info -> Some info
        | _ -> None

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
        match exp with
        | MethodCall m ->
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
    let leafKeys = System.Collections.Generic.Dictionary<string, int>()
    let sqlExprCounter = ref 0

    let paramArray = Expression.Parameter(typeof<obj[]>, "leafValues")

    let getOrAddLeaf (key: string) (mkLeaf: int -> ExprLeaf) (leafType: Type) =
        match leafKeys.TryGetValue(key) with
        | true, existingIdx ->
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

    let isGeneratedRecordType (t: Type) =
        t <> null && t.DeclaringType <> null && FSharp.Reflection.FSharpType.IsRecord(t, System.Reflection.BindingFlags.Public ||| System.Reflection.BindingFlags.NonPublic)

    let isOptionOfGeneratedRecordType (t: Type) =
        t <> null && isOptionType t && isGeneratedRecordType (t.GetGenericArguments().[0])

    let isTableType (t: Type) =
        isGeneratedRecordType t || isOptionOfGeneratedRecordType t

    let isSqlHydraMethod (m: MethodInfo) =
        m.Module.Name = "SqlHydra.Query.dll"

    let paramToAlias = System.Collections.Generic.Dictionary<ParameterExpression, string>()
    let typeToAlias = System.Collections.Generic.Dictionary<Type, string>()
    let outerParamNames = System.Collections.Generic.HashSet<string>()
    let outerParamTypes = System.Collections.Generic.Dictionary<string, Type>()
    let mutable aliasUsage = System.Collections.Generic.Dictionary<string, AliasUsage>()

    let rec unwrapBodyWithParams (exp: Expression) =
        match exp with
        | Lambda x ->
            for p in x.Parameters do
                outerParamNames.Add(p.Name) |> ignore
                outerParamTypes.[p.Name] <- p.Type
                if isTableType p.Type then
                    typeToAlias.[p.Type] <- p.Name
                    if isOptionType p.Type then
                        typeToAlias.[p.Type.GetGenericArguments().[0]] <- p.Name
            unwrapBodyWithParams x.Body
        | MethodCall m when m.Method.Name = "Invoke" -> unwrapBodyWithParams m.Object
        | _ -> exp

    let resolveTableAlias (p: ParameterExpression) =
        if outerParamNames.Contains(p.Name) then p.Name
        else
            match paramToAlias.TryGetValue(p) with
            | true, alias -> alias
            | false, _ ->
                match typeToAlias.TryGetValue(p.Type) with
                | true, alias -> alias
                | false, _ -> p.Name

    let rec isTableExpr (e: Expression) =
        match e with
        | Parameter p -> isTableType p.Type
        | Member inner when inner.Member.DeclaringType <> null && inner.Member.DeclaringType |> isOptionOrNullableType ->
            isTableExpr inner.Expression
        | _ -> false

    let rec resolveAliasFromExpr (e: Expression) =
        match e with
        | Parameter p -> resolveTableAlias p
        | Member inner when inner.Member.DeclaringType <> null && inner.Member.DeclaringType |> isOptionOrNullableType ->
            resolveAliasFromExpr inner.Expression
        | _ -> visitAlias e

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
        | AggregateColumn (aggType, (p, _)) ->
            let alias = visitAlias p.Expression
            let fqCol = $"[{alias}].[{p.Member.Name}]"
            let sqlFragment = $"{aggType}({fqCol})"
            let exprAlias = $"__hydra_expr_{!sqlExprCounter}"
            sqlExprCounter := !sqlExprCounter + 1
            let key = $"__sqlfn:{sqlFragment}"
            getOrAddLeaf key
                (fun idx -> SqlExprLeaf (sqlFragment, exp.Type, exprAlias, idx))
                exp.Type

        | MethodCall m when isSqlHydraMethod m.Method ->
            rewriteSqlFunction m exp

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
                        let newArgs = m.Arguments |> Seq.map rewrite |> Seq.toArray
                        let newObj = if m.Object <> null then rewrite m.Object else null
                        let argsChanged = Seq.zip m.Arguments newArgs |> Seq.exists (fun (a, b) -> not (obj.ReferenceEquals(a, b)))
                        let objChanged = m.Object <> null && not (obj.ReferenceEquals(newObj, m.Object))
                        if argsChanged || objChanged then
                            if m.Object <> null then Expression.Call(newObj, m.Method, newArgs) :> Expression
                            else Expression.Call(m.Method, newArgs) :> Expression
                        else exp
                | _ ->
                    let newArgs = m.Arguments |> Seq.map rewrite |> Seq.toArray
                    let newObj = if m.Object <> null then rewrite m.Object else null
                    let argsChanged = Seq.zip m.Arguments newArgs |> Seq.exists (fun (a, b) -> not (obj.ReferenceEquals(a, b)))
                    let objChanged = m.Object <> null && not (obj.ReferenceEquals(newObj, m.Object))
                    if argsChanged || objChanged then
                        if m.Object <> null then Expression.Call(newObj, m.Method, newArgs) :> Expression
                        else Expression.Call(m.Method, newArgs) :> Expression
                    else exp
            else
                let newArgs = m.Arguments |> Seq.map rewrite |> Seq.toArray
                let newObj = if m.Object <> null then rewrite m.Object else null
                let argsChanged = Seq.zip m.Arguments newArgs |> Seq.exists (fun (a, b) -> not (obj.ReferenceEquals(a, b)))
                let objChanged = m.Object <> null && not (obj.ReferenceEquals(newObj, m.Object))
                if argsChanged || objChanged then
                    if m.Object <> null then Expression.Call(newObj, m.Method, newArgs) :> Expression
                    else Expression.Call(m.Method, newArgs) :> Expression
                else exp

        | Lambda x ->
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

        | Parameter p when isTableType p.Type ->
            let alias = resolveTableAlias p
            let needsFullRecord =
                match aliasUsage.TryGetValue(alias) with
                | true, usage -> usage.RequiresFullRecord
                | false, _ -> true
            if needsFullRecord then
                let key = $"__table:{alias}"
                getOrAddLeaf key
                    (fun idx -> TableLeaf (alias, p.Type, idx))
                    p.Type
            else
                exp

        | Parameter _ -> exp

        | Member m when m.Member.DeclaringType <> null && m.Member.DeclaringType |> isOptionOrNullableType ->
            let newExpr = rewrite m.Expression
            if obj.ReferenceEquals(newExpr, m.Expression) then exp
            else Expression.MakeMemberAccess(newExpr, m.Member) :> Expression

        | Member m when m.Expression <> null && isTableExpr m.Expression ->
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
                let isOptional =
                    m.Type.IsGenericType && m.Type.GetGenericTypeDefinition() = typedefof<Option<_>>
                let isNullable =
                    m.Type.IsGenericType && m.Type.GetGenericTypeDefinition() = typedefof<Nullable<_>>
                let isOuterOptional =
                    match outerParamTypes.TryGetValue(alias) with
                    | true, outerType -> isOptionType outerType
                    | false, _ -> false
                let finalOptional = isOptional || isOuterOptional
                let colType =
                    if isOuterOptional && not isOptional && not isNullable then
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
        let rec visitSqlFnWithProvenance (exp: Expression) : string =
            match exp with
            | MethodCall m when m.Method.Name = "caseWhenMulti" ->
                let branches = extractListItemsProv m.Arguments.[0]
                let elseValue = renderExprAsSqlProv m.Arguments.[1]
                let whenClauses =
                    branches
                    |> List.map (fun (cond, value) -> $"WHEN {cond} THEN {value}")
                    |> String.concat " "
                $"CASE {whenClauses} ELSE {elseValue} END"
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
                    NormalizedPatterns.renderAggregate aggType fqCol
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

        and extractListItemsProv (exp: Expression) : (string * string) list =
            match exp with
            | :? MethodCallExpression as m when m.Method.Name = "Cons" || m.Method.Name = "op_ColonColon" ->
                let tupleArg = m.Arguments.[0]
                let restArg = m.Arguments.[1]
                let (cond, value) = extractTupleItemProv tupleArg
                (cond, value) :: extractListItemsProv restArg
            | :? NewExpression as n when n.Arguments.Count = 2 ->
                let tupleArg = n.Arguments.[0]
                let restArg = n.Arguments.[1]
                let (cond, value) = extractTupleItemProv tupleArg
                (cond, value) :: extractListItemsProv restArg
            | :? MemberExpression as m when m.Member.Name = "Empty" -> []
            | :? DefaultExpression -> []
            | _ ->
                try
                    let value = compileAndEvaluateExpression exp
                    match value with
                    | :? System.Collections.IEnumerable as items ->
                        [ for item in items do
                            let t = item.GetType()
                            let cond = t.GetProperty("Item1").GetValue(item) :?> bool
                            let value = t.GetProperty("Item2").GetValue(item)
                            let condStr = if cond then "TRUE" else "FALSE"
                            let valStr = match value with | :? string as s -> $"'{s}'" | v -> sprintf "%O" v
                            yield (condStr, valStr) ]
                    | _ -> notImplMsg $"Cannot extract list items from: {exp.NodeType}"
                with ex -> notImplMsg $"Cannot extract list items from: {exp.NodeType}. Inner: {ex.Message}"

        and extractTupleItemProv (exp: Expression) : string * string =
            match exp with
            | :? NewExpression as n when n.Arguments.Count = 2 ->
                let cond = renderExprAsSqlProv n.Arguments.[0]
                let value = renderExprAsSqlProv n.Arguments.[1]
                (cond, value)
            | :? MethodCallExpression as m when m.Method.Name = "NewTuple" ->
                let cond = renderExprAsSqlProv m.Arguments.[0]
                let value = renderExprAsSqlProv m.Arguments.[1]
                (cond, value)
            | _ -> notImplMsg $"Cannot extract tuple from: {exp.NodeType}"

        let sqlFragment = visitSqlFnWithProvenance (m :> Expression)
        let exprAlias = $"__hydra_expr_{!sqlExprCounter}"
        sqlExprCounter := !sqlExprCounter + 1
        let key = $"__sqlfn:{sqlFragment}"
        getOrAddLeaf key
            (fun idx -> SqlExprLeaf (sqlFragment, originalExp.Type, exprAlias, idx))
            originalExp.Type

    let actualBody = unwrapBodyWithParams (selectExpression :> Expression)

    aliasUsage <- analyzeProjectionShape actualBody outerParamNames outerParamTypes typeToAlias

    let rewrittenBody = rewrite actualBody

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
