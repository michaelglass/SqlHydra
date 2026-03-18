/// Linq select query builders
[<AutoOpen>]
module SqlHydra.Query.SelectBuilders

open System
open System.Linq.Expressions
open System.Data.Common
open System.Threading
open System.Threading.Tasks
open SqlKata

/// The context type that determines how the query context is created and disposed.
/// Can be implicitly converted from a QueryContext, a function that creates a QueryContext, a Task that creates a QueryContext, or an Async that creates a QueryContext.
type ContextType =
    /// A new QueryContext will be created and disposed within the select builder.
    | Create of create: (unit -> QueryContext)
    /// A new QueryContext will be created and disposed within the select builder.
    | CreateTask of create: (unit -> Task<QueryContext>)
    /// A new QueryContext will be created and disposed within the select builder.
    | CreateAsync of create: (unit -> Async<QueryContext>)
    /// A shared QueryContext will be used and not disposed within the select builder.
    | Shared of QueryContext
    static member op_Implicit(ctx: QueryContext) = Shared ctx
    static member op_Implicit(createFn: unit -> QueryContext) = Create createFn
    static member op_Implicit(createFn: unit -> Task<QueryContext>) = CreateTask createFn
    static member op_Implicit(createFn: unit -> Async<QueryContext>) = CreateAsync createFn

/// SRTP-based context type resolution for selectTask/selectAsync
[<RequireQualifiedAccess>]
module ContextTypeResolver =

    /// Helper type for SRTP overload resolution using the $ operator pattern
    type Resolver =
        | Resolver

        // Direct ContextType - pass through
        static member inline ($) (Resolver, ct: ContextType) = ct

        // QueryContext - wrap in Shared
        static member inline ($) (Resolver, ctx: QueryContext) = Shared ctx

        // unit -> QueryContext - wrap in Create
        static member inline ($) (Resolver, createFn: unit -> QueryContext) = Create createFn

        // unit -> Task<QueryContext> - wrap in CreateTask
        static member inline ($) (Resolver, createFn: unit -> Task<QueryContext>) = CreateTask createFn

        // unit -> Async<QueryContext> - wrap in CreateAsync
        static member inline ($) (Resolver, createFn: unit -> Async<QueryContext>) = CreateAsync createFn

        // Explicit overload for IQueryContextFactory
        static member inline ($) (Resolver, factory: IQueryContextFactory) =
            CreateTask factory.OpenContextAsync

    /// Inline function that resolves any supported type to ContextType
    let inline resolve< ^T when (Resolver or ^T) : (static member ($) : Resolver * ^T -> ContextType)> (input: ^T) : ContextType =
        Resolver $ input

module ContextUtils = 
    let private tryOpen (ctx: QueryContext) = 
        if ctx.Connection.State <> Data.ConnectionState.Open 
        then ctx.Connection.Open()
        ctx

    let getContext ct : Task<QueryContext> =        
        match ct with 
        | Create create ->             
            create() |> tryOpen |> Task.FromResult
        | CreateTask create -> 
            task {
                let! ctx = create() 
                return ctx |> tryOpen
            }
        | CreateAsync create -> 
            task {
                let! ctx = create() 
                return ctx |> tryOpen
            }
        | Shared ctx ->             
            ctx |> tryOpen |> Task.FromResult

    let disposeIfNotShared ct (ctx: QueryContext) =
        match ct with
        | Create _ -> (ctx :> IDisposable).Dispose()
        | CreateTask _ -> (ctx :> IDisposable).Dispose()
        | CreateAsync _ -> (ctx :> IDisposable).Dispose()
        | Shared _ -> () // Do not dispose if shared


[<RequireQualifiedAccess>]
module ResultModifier =
    type ModifierBase<'T>(qs: QuerySource<'T, Query>) = 
        member this.Query = qs.Query

    type Count<'T>(qs) = inherit ModifierBase<'T>(qs)

    type Head<'T>(qs) = inherit ModifierBase<'T>(qs)

/// The base select builder that contains all common operations
type SelectBuilder<'Selected, 'Mapped> () =

    let getQueryOrDefault (state: QuerySource<'T>) =
        match state with
        | :? QuerySource<'T, Query> as qs -> qs.Query
        | _ -> Query()            

    let mergeTableMappings (a: Map<TableMappingKey, TableMapping>, b: Map<TableMappingKey, TableMapping>) =
        Map (Seq.concat [ (Map.toSeq a); (Map.toSeq b) ])
            
    let qualifyColumnWithAlias (alias: string) (col: Reflection.MemberInfo) = 
        $"%s{alias}.%s{col.Name}"

    member val MapFn = Option<Func<'Selected, 'Mapped>>.None with get, set
    member val CancellationToken = CancellationToken.None with get, set
    
    member this.For (state: QuerySource<'T>, [<ReflectedDefinition>] forExpr: FSharp.Quotations.Expr<'T -> QuerySource<'T>>) =
        let tableAlias = QuotationVisitor.visitFor forExpr
        let query = state |> getQueryOrDefault
        let tblMaybe, tableMappings = TableMappings.tryGetByRootOrAlias tableAlias state.TableMappings

        match tblMaybe with
        | Some tbl when tbl.IsCte ->
            let q = query.From($"{tbl.Name} as {tableAlias}")
            match tbl.CteQuery with
            | Some cteQuery -> QuerySource<'T, Query>(q.With(tbl.Name, cteQuery), tableMappings)
            | None -> QuerySource<'T, Query>(q, tableMappings)
        | Some tbl ->
            QuerySource<'T, Query>(query.From($"{tbl.Schema}.{tbl.Name} as {tableAlias}"), tableMappings)
        | None ->
            // Handles this scenario: `select (p.FirstName, p.LastName) into (fname, lname)`
            state :?> QuerySource<'T, Query>

    member this.Yield _ =
        QuerySource<'T>(Map.empty)

    // Prevents errors while typing join statement if rest of query is not filled in yet.
    member this.Zero _ = 
        QuerySource<'T>(Map.empty)

    /// Provides direct access to the underlying SqlKata.Query.
    [<CustomOperation("kata", MaintainsVariableSpace = true)>]
    member this.Kata (state: QuerySource<'T, Query>, kata) = 
        let query = state.Query
        QuerySource<'T, Query>(query |> kata, state.TableMappings)

    /// Sets the WHERE condition
    [<CustomOperation("where", MaintainsVariableSpace = true)>]
    member this.Where (state: QuerySource<'T, Query>, [<ProjectionParameter>] whereExpression) = 
        let query = state.Query
        let tableMappings = state.TableMappings |> Map.values
        let where = LinqExpressionVisitors.visitWhere<'T> tableMappings whereExpression qualifyColumnWithAlias
        QuerySource<'T, Query>(query.Where(fun w -> where), state.TableMappings)

    /// WHERE NOT EXISTS (subquery)
    [<CustomOperation("whereNotExists", MaintainsVariableSpace = true)>]
    member this.WhereNotExists (state: QuerySource<'T, Query>, subquery: SelectQuery) =
        QuerySource<'T, Query>(state.Query.WhereNotExists(subquery.ToKataQuery()), state.TableMappings)

    /// WHERE EXISTS (subquery)
    [<CustomOperation("whereExists", MaintainsVariableSpace = true)>]
    member this.WhereExists (state: QuerySource<'T, Query>, subquery: SelectQuery) =
        QuerySource<'T, Query>(state.Query.WhereExists(subquery.ToKataQuery()), state.TableMappings)

    /// Sets the SELECT statement and filters the query to include only the selected tables
    [<CustomOperation("select", MaintainsVariableSpace = true, AllowIntoPattern = true)>]
    member this.Select (state: QuerySource<'T, Query>, [<ProjectionParameter>] selectExpression: Expression<Func<'T, 'Selected>>) =
        let selections = LinqExpressionVisitors.visitSelect<'T,'Selected> selectExpression

        let queryWithSelectedColumns =
            selections
            |> List.fold (fun (q: Query) -> function
                | LinqExpressionVisitors.SelectedTable (tableAlias, tableType) ->
                    // Explicitly select all columns in generated table record type.
                    // This avoids table scans due to 'SELECT *', and avoids potential errors when a table has more columns than expected.
                    //let props =
                    //    FSharp.Reflection.FSharpType.GetRecordFields(tableType)
                    //    |> Array.map (fun p -> $"%s{tableAlias}.%s{p.Name}")
                    //q.Select(props)

                    // Bug fix: temporarily revert to * until option types are properly implemented.
                    // `tableType` was not properly unwrapping option types, causing a runtime error.
                    // For example, left joining a table creates an option type, which should be unwrapped.
                    q.Select($"%s{tableAlias}.*")

                | LinqExpressionVisitors.SelectedColumn (tableAlias, column, _, _, _) ->
                    // Select a single column
                    q.Select($"%s{tableAlias}.%s{column}")
                | LinqExpressionVisitors.SelectedExpression sqlFragment ->
                    q.SelectRaw(sqlFragment)
                | LinqExpressionVisitors.SelectedParameter value ->
                    q.SelectRaw("?", [| value |])
            ) state.Query

        QuerySource<'Selected, Query>(queryWithSelectedColumns, state.TableMappings)

    /// Future v4.0 enhancement.
    /// Sets the SELECT statement using an arbitrary F# expression.
    /// Supports string interpolation, conditionals, and other F# expressions that reference DB columns.
    //[<CustomOperation("select", MaintainsVariableSpace = true, AllowIntoPattern = true)>]
    //member this.SelectExpr (state: QuerySource<'T, Query>, [<ProjectionParameter>] selectExpression: Expression<Func<'T, 'Selected>>) =
    //    let exprInfo = SelectExprVisitors.visitSelectExpr<'T, 'Selected> selectExpression

    //    // Collect aliases that have a TableLeaf (full record); suppress individual ColumnLeafs for those aliases
    //    let tableLeafAliases =
    //        exprInfo.Leaves
    //        |> List.choose (function
    //            | SelectExprVisitors.TableLeaf (tableAlias, _, _) -> Some tableAlias
    //            | _ -> None)
    //        |> Set.ofList

    //    let queryWithSelectedColumns =
    //        exprInfo.Leaves
    //        |> List.fold (fun (q: Query) leaf ->
    //            match leaf with
    //            | SelectExprVisitors.TableLeaf (tableAlias, _, _) -> q.Select($"%s{tableAlias}.*")
    //            | SelectExprVisitors.ColumnLeaf (tableAlias, _, _, _, _, _) when tableLeafAliases.Contains(tableAlias) -> q // Suppressed by TableLeaf
    //            | SelectExprVisitors.ColumnLeaf (tableAlias, column, _, _, _, _) -> q.Select($"%s{tableAlias}.%s{column}")
    //            | SelectExprVisitors.SqlExprLeaf (sqlFragment, _, alias, _) -> q.SelectRaw($"{sqlFragment} AS {alias}")
    //        ) state.Query

    //    SelectExprVisitors.SelectExprStore.set queryWithSelectedColumns exprInfo
    //    QuerySource<'Selected, Query>(queryWithSelectedColumns, state.TableMappings)

    /// Sets the ORDER BY for single column
    [<CustomOperation("orderBy", MaintainsVariableSpace = true)>]
    member this.OrderBy (state: QuerySource<'T, Query>, [<ProjectionParameter>] propertySelector) = 
        let orderedQuery = 
            LinqExpressionVisitors.visitOrderByPropertySelector<'T, 'Prop> propertySelector
            |> function 
                | LinqExpressionVisitors.OrderByColumn (tableAlias, p) -> 
                    let fqCol = $"%s{tableAlias}.%s{p.Name}"
                    state.Query.OrderBy(fqCol)
                | LinqExpressionVisitors.OrderByAggregateColumn (aggType, tableAlias, p) ->
                    let fqCol = $"%s{tableAlias}.%s{p.Name}"
                    let aggFragment = if aggType = "COUNTDISTINCT" then $"COUNT(DISTINCT %s{fqCol})" else $"%s{aggType}(%s{fqCol})"
                    state.Query.OrderByRaw(aggFragment)
                | LinqExpressionVisitors.OrderByIgnored ->
                    state.Query
        QuerySource<'T, Query>(orderedQuery, state.TableMappings)

    /// Sets the ORDER BY for single column
    [<CustomOperation("thenBy", MaintainsVariableSpace = true)>]
    member this.ThenBy (state: QuerySource<'T, Query>, [<ProjectionParameter>] propertySelector) =
        this.OrderBy(state, propertySelector)

    /// Sets the ORDER BY DESC for single column
    [<CustomOperation("orderByDescending", MaintainsVariableSpace = true)>]
    member this.OrderByDescending (state: QuerySource<'T, Query>, [<ProjectionParameter>] propertySelector) =
        let orderedQuery =
            LinqExpressionVisitors.visitOrderByPropertySelector<'T, 'Prop> propertySelector
            |> function
                | LinqExpressionVisitors.OrderByColumn (tableAlias, p) ->
                    let fqCol = $"%s{tableAlias}.%s{p.Name}"
                    state.Query.OrderByDesc(fqCol)
                | LinqExpressionVisitors.OrderByAggregateColumn (aggType, tableAlias, p) ->
                    let fqCol = $"%s{tableAlias}.%s{p.Name}"
                    let aggFragment = if aggType = "COUNTDISTINCT" then $"COUNT(DISTINCT %s{fqCol})" else $"%s{aggType}(%s{fqCol})"
                    state.Query.OrderByRaw($"%s{aggFragment} DESC")
                | LinqExpressionVisitors.OrderByIgnored -> 
                    state.Query
        QuerySource<'T, Query>(orderedQuery, state.TableMappings)

    /// Sets the ORDER BY DESC for single column
    [<CustomOperation("thenByDescending", MaintainsVariableSpace = true)>]
    member this.ThenByDescending (state: QuerySource<'T, Query>, [<ProjectionParameter>] propertySelector) = 
        this.OrderByDescending(state, propertySelector)

    /// Sets the SKIP value for query
    [<CustomOperation("skip", MaintainsVariableSpace = true)>]
    member this.Skip (state: QuerySource<'T, Query>, skip) = 
        QuerySource<'T, Query>(state.Query.Skip(skip), state.TableMappings)
    
    /// Sets the TAKE value for query
    [<CustomOperation("take", MaintainsVariableSpace = true)>]
    member this.Take (state: QuerySource<'T, Query>, take) =
        QuerySource<'T, Query>(state.Query.Take(take), state.TableMappings)

    /// INNER JOIN table on one or more columns
    [<CustomOperation("join", MaintainsVariableSpace = true, IsLikeJoin = true, JoinConditionWord = "on")>]
    member this.Join (outerSource: QuerySource<'Outer>, 
                      innerSource: QuerySource<'Inner>, 
                      outerKeySelector: Expression<Func<'Outer,'Key>>, 
                      innerKeySelector: Expression<Func<'Inner,'Key>>, 
                      resultSelector: Expression<Func<'Outer,'Inner,'JoinResult>> ) = 

        let outerProperties = LinqExpressionVisitors.visitJoin<'Outer, 'Key> outerKeySelector // left
        let innerProperties = LinqExpressionVisitors.visitJoin<'Inner, 'Key> innerKeySelector // right

        let mergedTables = 
            // Update outer table mappings with join aliases (accumulated outer/left mappings)
            let outerTableMappings = 
                outerProperties
                |> List.fold (fun (mappings: Map<TableMappingKey, TableMapping>) joinPI -> 
                    let _, updatedMappings = TableMappings.tryGetByRootOrAlias joinPI.Alias mappings
                    updatedMappings
                ) outerSource.TableMappings

            // Update inner table mapping with join aliases (this will always be 1 mapping being joined)
            let innerTableMappings = 
                innerProperties
                |> List.fold (fun (mappings: Map<TableMappingKey, TableMapping>) joinPI -> 
                    let _, updatedMappings = TableMappings.tryGetByRootOrAlias joinPI.Alias mappings
                    updatedMappings
                ) innerSource.TableMappings
        
            mergeTableMappings (outerTableMappings, innerTableMappings)

        let outerQuery = outerSource |> getQueryOrDefault
        let innerTableNameAsAlias = 
            innerProperties 
            |> Seq.map (fun p -> p, mergedTables[TableAliasKey p.Alias])
            |> Seq.map (fun (p, tbl) -> $"%s{tbl.Schema}.%s{tbl.Name} AS %s{p.Alias}")
            |> Seq.head
        
        let joinOn = 
            List.zip outerProperties innerProperties
            |> List.fold (fun (j: Join) (outerProp, innerProp) -> 
                j.On($"%s{outerProp.Alias}.%s{outerProp.Member.Name}", $"%s{innerProp.Alias}.%s{innerProp.Member.Name}")
            ) (Join())
            
        QuerySource<'JoinResult, Query>(outerQuery.Join(innerTableNameAsAlias, fun j -> joinOn), mergedTables)

    /// LEFT JOIN table on one or more columns
    [<CustomOperation("leftJoin", MaintainsVariableSpace = true, IsLikeJoin = true, JoinConditionWord = "on")>]
    member this.LeftJoin (outerSource: QuerySource<'Outer>, 
                          innerSource: QuerySource<'Inner>, 
                          outerKeySelector: Expression<Func<'Outer,'Key>>, 
                          innerKeySelector: Expression<Func<'Inner option,'Key>>, 
                          resultSelector: Expression<Func<'Outer,'Inner option,'JoinResult>> ) = 

        let outerProperties = LinqExpressionVisitors.visitJoin<'Outer, 'Key> outerKeySelector
        let innerProperties = LinqExpressionVisitors.visitJoin<'Inner option, 'Key> innerKeySelector
        
        let mergedTables = 
            // Update outer table mappings with join aliases
            let outerTableMappings = 
                outerProperties
                |> List.fold (fun (mappings: Map<TableMappingKey, TableMapping>) joinPI -> 
                    let _, updatedMappings = TableMappings.tryGetByRootOrAlias joinPI.Alias mappings
                    updatedMappings
                ) outerSource.TableMappings

            // Update inner table mappings with join aliases
            let innerTableMappings = 
                innerProperties
                |> List.fold (fun (mappings: Map<TableMappingKey, TableMapping>) joinPI -> 
                    let _, updatedMappings = TableMappings.tryGetByRootOrAlias joinPI.Alias mappings
                    updatedMappings
                ) innerSource.TableMappings
        
            mergeTableMappings (outerTableMappings, innerTableMappings)

        let outerQuery = outerSource |> getQueryOrDefault
        let innerTableNameAsAlias = 
            innerProperties 
            |> Seq.map (fun p -> p, mergedTables[TableAliasKey p.Alias])
            |> Seq.map (fun (p, tbl) -> $"%s{tbl.Schema}.%s{tbl.Name} AS %s{p.Alias}")
            |> Seq.head

        let joinOn = 
            List.zip outerProperties innerProperties
            |> List.fold (fun (j: Join) (outerProp, innerProp) -> 
                j.On($"%s{outerProp.Alias}.%s{outerProp.Member.Name}", $"%s{innerProp.Alias}.%s{innerProp.Member.Name}")
            ) (Join())
            
        QuerySource<'JoinResult, Query>(outerQuery.LeftJoin(innerTableNameAsAlias, fun j -> joinOn), mergedTables)

    /// References a table variable from a correlated parent query from within a subquery.
    [<CustomOperation("correlate", MaintainsVariableSpace = true, IsLikeZip = true)>]
    member this.Correlate (outerSource: QuerySource<'Outer>,
                      innerSource: QuerySource<'Inner>,
                      resultSelector: Expression<Func<'Outer,'Inner,'JoinResult>> ) =

        let mergedTables = mergeTableMappings (outerSource.TableMappings, innerSource.TableMappings)
        let query = outerSource |> getQueryOrDefault
        QuerySource<'JoinResult, Query>(query, mergedTables)

    /// Introduces an INNER JOIN table binding (use with on' to complete the join).
    /// Unlike the standard `join ... on`, this allows predicate-style join conditions.
    /// Example: `join' d in Sales.Detail; on' (o.Id = d.Id && d.Type = "X")`
    [<CustomOperation("join'", MaintainsVariableSpace = true, IsLikeZip = true)>]
    member this.Join' (outerSource: QuerySource<'Outer>,
                        innerSource: QuerySource<'Inner>,
                        resultSelector: Expression<Func<'Outer, 'Inner, 'JoinResult>>) =
        // Extract alias from the resultSelector's second parameter (the inner table alias)
        let innerAlias =
            match resultSelector.Parameters |> Seq.toList with
            | [_; inner] -> inner.Name
            | _ -> failwith "Expected two parameters in join result selector"

        // Merge table mappings
        let _, innerTableMappings = TableMappings.tryGetByRootOrAlias innerAlias innerSource.TableMappings
        let mergedTables = mergeTableMappings (outerSource.TableMappings, innerTableMappings)

        // Get inner table info
        let innerTable = mergedTables[TableAliasKey innerAlias]
        let tableName =
            if innerTable.Schema = "" then innerTable.Name
            else $"{innerTable.Schema}.{innerTable.Name}"

        let pendingJoin = {
            JoinType = JoinType.Inner
            TableName = tableName
            TableAlias = innerAlias
        }

        let query = outerSource |> getQueryOrDefault
        // Store pending join info associated with this query
        PendingJoins.set query pendingJoin
        QuerySource<'JoinResult, Query>(query, mergedTables)

    /// Introduces a LEFT JOIN table binding (use with on' to complete the join).
    /// Unlike the standard `leftJoin ... on`, this allows predicate-style join conditions.
    /// Example: `leftJoin' d in Sales.Detail; on' (o.Id = d.Value.Id && d.Value.Type = "X")`
    [<CustomOperation("leftJoin'", MaintainsVariableSpace = true, IsLikeZip = true)>]
    member this.LeftJoin' (outerSource: QuerySource<'Outer>,
                            innerSource: QuerySource<'Inner>,
                            resultSelector: Expression<Func<'Outer, 'Inner option, 'JoinResult>>) =
        // Extract alias from the resultSelector's second parameter (the inner table alias)
        let innerAlias =
            match resultSelector.Parameters |> Seq.toList with
            | [_; inner] -> inner.Name
            | _ -> failwith "Expected two parameters in leftJoin result selector"

        // Merge table mappings
        let _, innerTableMappings = TableMappings.tryGetByRootOrAlias innerAlias innerSource.TableMappings
        let mergedTables = mergeTableMappings (outerSource.TableMappings, innerTableMappings)

        // Get inner table info
        let innerTable = mergedTables[TableAliasKey innerAlias]
        let tableName =
            if innerTable.Schema = "" then innerTable.Name
            else $"{innerTable.Schema}.{innerTable.Name}"

        let pendingJoin = {
            JoinType = JoinType.Left
            TableName = tableName
            TableAlias = innerAlias
        }

        let query = outerSource |> getQueryOrDefault
        // Store pending join info associated with this query
        PendingJoins.set query pendingJoin
        QuerySource<'JoinResult, Query>(query, mergedTables)

    /// Completes a pending join with a predicate expression.
    /// Used after `join'` or `leftJoin'` to specify the join condition.
    /// Example: `on' (o.Id = d.Id && d.Type = "X")`
    [<CustomOperation("on'", MaintainsVariableSpace = true)>]
    member this.OnPredicate (state: QuerySource<'T, Query>,
                             [<ProjectionParameter>] joinPredicate: Expression<Func<'T, bool>>) =
        let query = state.Query
        let pendingJoin =
            match PendingJoins.tryTake query with
            | Some pj -> pj
            | None -> failwith "on' must be used after join' or leftJoin'"

        let tableMappings = state.TableMappings |> Map.values

        // Build the join predicate visitor
        let joinBuilder = LinqExpressionVisitors.visitJoinPredicate<'T> tableMappings joinPredicate qualifyColumnWithAlias

        // Create the table name with alias
        let tableNameAsAlias = $"{pendingJoin.TableName} AS {pendingJoin.TableAlias}"

        // Apply the join based on type
        let updatedQuery =
            match pendingJoin.JoinType with
            | JoinType.Inner ->
                query.Join(tableNameAsAlias, fun j -> joinBuilder j)
            | JoinType.Left ->
                query.LeftJoin(tableNameAsAlias, fun j -> joinBuilder j)

        // After applying the join, register CTE if needed
        let finalQuery =
            let innerTable = state.TableMappings |> Map.tryFind (TableAliasKey pendingJoin.TableAlias)
            match innerTable with
            | Some tbl when tbl.IsCte ->
                match tbl.CteQuery with
                | Some cteQuery -> updatedQuery.With(tbl.Name, cteQuery)
                | None -> updatedQuery
            | _ -> updatedQuery

        QuerySource<'T, Query>(finalQuery, state.TableMappings)

    /// Sets the GROUP BY for one or more columns.
    [<CustomOperation("groupBy", MaintainsVariableSpace = true)>]
    member this.GroupBy (state: QuerySource<'T, Query>, [<ProjectionParameter>] propertySelector) = 
        let properties = LinqExpressionVisitors.visitPropertiesSelector<'T, 'Prop> propertySelector qualifyColumnWithAlias
        QuerySource<'T, Query>(state.Query.GroupBy(properties |> List.toArray), state.TableMappings)

    /// Sets the HAVING condition.
    [<CustomOperation("having", MaintainsVariableSpace = true)>]
    member this.Having (state: QuerySource<'T, Query>, [<ProjectionParameter>] havingExpression) =
        let tableMappings = state.TableMappings |> Map.values
        let having = LinqExpressionVisitors.visitHaving<'T> tableMappings havingExpression qualifyColumnWithAlias
        QuerySource<'T, Query>(state.Query.Having(fun w -> having), state.TableMappings)

    /// Sets query to return DISTINCT values
    [<CustomOperation("distinct", MaintainsVariableSpace = true)>]
    member this.Distinct (state: QuerySource<'T, Query>) =
        QuerySource<'T, Query>(state.Query.Distinct(), state.TableMappings)

    /// Sets a CancellationToken for the query execution.
    [<CustomOperation("cancel", MaintainsVariableSpace = true)>]
    member this.Cancel (state: QuerySource<'T, Query>, cancellationToken: CancellationToken) =
        this.CancellationToken <- cancellationToken
        state

    /// Maps the query results into a seq.
    [<CustomOperation("mapSeq", MaintainsVariableSpace = true)>]
    member this.MapSeq (state: QuerySource<'Selected, Query>, [<ProjectionParameter>] map: Func<'Selected, 'Mapped>) =
        this.MapFn <- Some map
        QuerySource<'Mapped seq, Query>(state.Query, state.TableMappings)
    
    /// Maps the query results into an array.
    [<CustomOperation("mapArray", MaintainsVariableSpace = true)>]
    member this.MapArray (state: QuerySource<'Selected, Query>, [<ProjectionParameter>] map: Func<'Selected, 'Mapped>) =
        this.MapFn <- Some map
        QuerySource<'Mapped array, Query>(state.Query, state.TableMappings)
        
    /// Maps the query results into a list.
    [<CustomOperation("mapList", MaintainsVariableSpace = true)>]
    member this.MapList (state: QuerySource<'Selected, Query>, [<ProjectionParameter>] map: Func<'Selected, 'Mapped>) =
        this.MapFn <- Some map
        QuerySource<'Mapped list, Query>(state.Query, state.TableMappings)
    
    /// Returns the query results as an array.
    [<CustomOperation("toArray", MaintainsVariableSpace = true)>]
    member this.ToArray (state: QuerySource<'Selected, Query>) =
        QuerySource<'Selected array, Query>(state.Query, state.TableMappings)

    /// Returns the query results as a list.
    [<CustomOperation("toList", MaintainsVariableSpace = true)>]
    member this.ToList (state: QuerySource<'Selected, Query>) =
        QuerySource<'Selected list, Query>(state.Query, state.TableMappings)

    /// COUNT aggregate function
    [<CustomOperation("count", MaintainsVariableSpace = true)>]
    member this.Count (state: QuerySource<'T, Query>) = 
        QuerySource<ResultModifier.Count<int>, Query>(state.Query.AsCount(), state.TableMappings)

    /// Applies Seq.tryHead to the 'Selected query results.
    [<CustomOperation("tryHead", MaintainsVariableSpace = true)>]
    member this.TryHead (state: QuerySource<'Selected, Query>) = 
        QuerySource<'Selected option, Query>(state.Query, state.TableMappings)

    /// Applies Seq.tryHead to the 'Mapped query results.
    [<CustomOperation("tryHead", MaintainsVariableSpace = true)>]
    member this.TryHead (state: QuerySource<'Mapped seq, Query>) = 
        QuerySource<'Mapped option, Query>(state.Query, state.TableMappings)
    
    /// Applies Seq.tryHead to the 'Mapped query results.
    [<CustomOperation("tryHead", MaintainsVariableSpace = true)>]
    member this.TryHead (state: QuerySource<'Mapped array, Query>) = 
        QuerySource<'Mapped option, Query>(state.Query, state.TableMappings)
        
    /// Applies Seq.tryHead to the 'Mapped query results.
    [<CustomOperation("tryHead", MaintainsVariableSpace = true)>]
    member this.TryHead (state: QuerySource<'Mapped list, Query>) = 
        QuerySource<'Mapped option, Query>(state.Query, state.TableMappings)

    /// Applies Seq.head to the 'Selected query results.
    [<CustomOperation("head", MaintainsVariableSpace = true)>]
    member this.Head (state: QuerySource<'Selected, Query>) = 
        QuerySource<ResultModifier.Head<'Selected>, Query>(state.Query, state.TableMappings)

    /// Applies Seq.head to the 'Mapped query results.
    [<CustomOperation("head", MaintainsVariableSpace = true)>]
    member this.Head (state: QuerySource<'Mapped seq, Query>) = 
        QuerySource<ResultModifier.Head<'Mapped>, Query>(state.Query, state.TableMappings)

    /// Applies Seq.head to the 'Selected query results.
    [<CustomOperation("head", MaintainsVariableSpace = true)>]
    member this.Head (state: QuerySource<'Mapped array, Query>) = 
        QuerySource<ResultModifier.Head<'Mapped>, Query>(state.Query, state.TableMappings)
    
    /// Applies Seq.head to the 'Selected query results.
    [<CustomOperation("head", MaintainsVariableSpace = true)>]
    member this.Head (state: QuerySource<'Mapped list, Query>) = 
        QuerySource<ResultModifier.Head<'Mapped>, Query>(state.Query, state.TableMappings)

/// A select builder that returns a select query.
type SelectQueryBuilder<'Selected, 'Mapped> () = 
    inherit SelectBuilder<'Selected, 'Mapped>()
    
    member this.Run (state: QuerySource<ResultModifier.Count<int>, Query>) = 
        SelectQuery<int>(state.Query)

    member this.Run (state: QuerySource<'Selected, Query>) =
        SelectQuery<'Selected>(state.Query)


/// A select builder that returns a Task result.
type SelectTaskBuilder<'Selected, 'Mapped> (ct: ContextType) =
    inherit SelectBuilder<'Selected, 'Mapped>()

    member this.RunSelected(query: Query, resultModifier) =
        task {
            let! ctx = ContextUtils.getContext ct
            try
                use cmd = ctx.BuildCommand(query)
                use! reader = cmd.ExecuteReaderAsync(this.CancellationToken)
                let readEntity = Hydration.buildRowReader<'Selected> ctx.Provider reader
                let results = ResizeArray<'Selected>()

                let! hasMore = reader.ReadAsync(this.CancellationToken)
                let mutable hasMore = hasMore
                while hasMore && not this.CancellationToken.IsCancellationRequested do
                    results.Add(readEntity())
                    let! hasMore' = reader.ReadAsync(this.CancellationToken)
                    hasMore <- hasMore'

                return results :> seq<'Selected> |> resultModifier
            finally
                ContextUtils.disposeIfNotShared ct ctx
        }

    member this.RunMapped(query: Query, resultModifier) =
        task {
            let! ctx = ContextUtils.getContext ct
            try
                use cmd = ctx.BuildCommand(query)
                use! reader = cmd.ExecuteReaderAsync(this.CancellationToken)
                let readEntity = Hydration.buildRowReader<'Selected> ctx.Provider reader
                let results = ResizeArray<'Mapped>()

                let! hasMore = reader.ReadAsync(this.CancellationToken)
                let mutable hasMore = hasMore
                while hasMore && not this.CancellationToken.IsCancellationRequested do
                    results.Add(this.MapFn.Value.Invoke(readEntity()))
                    let! hasMore' = reader.ReadAsync(this.CancellationToken)
                    hasMore <- hasMore'

                return results :> seq<'Mapped> |> resultModifier
            finally
                ContextUtils.disposeIfNotShared ct ctx
        }

    member private this.RunSelectExpr(query: Query, exprInfo: SelectExprVisitors.SelectExprInfo, resultModifier) =
        task {
            let! ctx = ContextUtils.getContext ct
            try
                use cmd = ctx.BuildCommand(query)
                use! reader = cmd.ExecuteReaderAsync(this.CancellationToken)
                let readRow = Hydration.buildSelectExprReader ctx.Provider reader exprInfo

                let results = ResizeArray<'Selected>()
                let! hasMore = reader.ReadAsync(this.CancellationToken)
                let mutable hasMore = hasMore
                while hasMore do
                    let fields = readRow()
                    let result = exprInfo.CompiledMapper.Invoke(fields) :?> 'Selected
                    results.Add(result)
                    let! hasMore' = reader.ReadAsync(this.CancellationToken)
                    hasMore <- hasMore'

                return results :> seq<'Selected> |> resultModifier
            finally
                ContextUtils.disposeIfNotShared ct ctx
        }

    member private this.RunSelectExprMapped(query: Query, exprInfo: SelectExprVisitors.SelectExprInfo, resultModifier) =
        task {
            let! ctx = ContextUtils.getContext ct
            try
                use cmd = ctx.BuildCommand(query)
                use! reader = cmd.ExecuteReaderAsync(this.CancellationToken)
                let readRow = Hydration.buildSelectExprReader ctx.Provider reader exprInfo

                let results = ResizeArray<'Mapped>()
                let! hasMore = reader.ReadAsync(this.CancellationToken)
                let mutable hasMore = hasMore
                while hasMore do
                    let fields = readRow()
                    let selected = exprInfo.CompiledMapper.Invoke(fields) :?> 'Selected
                    results.Add(this.MapFn.Value.Invoke(selected))
                    let! hasMore' = reader.ReadAsync(this.CancellationToken)
                    hasMore <- hasMore'

                return results :> seq<'Mapped> |> resultModifier
            finally
                ContextUtils.disposeIfNotShared ct ctx
        }

    /// Run: default
    /// Called when no mapSeq, mapArray or mapList is present;
    /// this input will always be 'Selected -- even if select is not present.
    member this.Run(state: QuerySource<'Selected, Query>) =
        match SelectExprVisitors.SelectExprStore.tryGet(state.Query) with
        | Some exprInfo -> this.RunSelectExpr(state.Query, exprInfo, id)
        | None -> this.RunSelected(state.Query, id)
    
    /// Run: toList
    member this.Run(state: QuerySource<'Selected list, Query>) =
        match SelectExprVisitors.SelectExprStore.tryGet(state.Query) with
        | Some exprInfo -> this.RunSelectExpr(state.Query, exprInfo, Seq.toList)
        | None -> this.RunSelected(state.Query, Seq.toList)

    /// Run: toArray
    member this.Run(state: QuerySource<'Selected array, Query>) =
        match SelectExprVisitors.SelectExprStore.tryGet(state.Query) with
        | Some exprInfo -> this.RunSelectExpr(state.Query, exprInfo, Seq.toArray)
        | None -> this.RunSelected(state.Query, Seq.toArray)

    /// Run: mapList
    member this.Run(state: QuerySource<'Mapped list, Query>) =
        match SelectExprVisitors.SelectExprStore.tryGet(state.Query) with
        | Some exprInfo -> this.RunSelectExprMapped(state.Query, exprInfo, Seq.toList)
        | None -> this.RunMapped(state.Query, Seq.toList)

    // Run: mapArray
    member this.Run(state: QuerySource<'Mapped array, Query>) =
        match SelectExprVisitors.SelectExprStore.tryGet(state.Query) with
        | Some exprInfo -> this.RunSelectExprMapped(state.Query, exprInfo, Seq.toArray)
        | None -> this.RunMapped(state.Query, Seq.toArray)

    // Run: mapSeq
    member this.Run(state: QuerySource<'Mapped seq, Query>) =
        match SelectExprVisitors.SelectExprStore.tryGet(state.Query) with
        | Some exprInfo -> this.RunSelectExprMapped(state.Query, exprInfo, id)
        | None -> this.RunMapped(state.Query, id)

    // Run: tryHead - 'Selected
    member this.Run(state: QuerySource<'Selected option, Query>) =
        match SelectExprVisitors.SelectExprStore.tryGet(state.Query) with
        | Some exprInfo -> this.RunSelectExpr(state.Query, exprInfo, Seq.tryHead)
        | None -> this.RunSelected(state.Query, Seq.tryHead)

    // Run: tryHead - 'Mapped
    member this.Run(state: QuerySource<'Mapped option, Query>) =
        match SelectExprVisitors.SelectExprStore.tryGet(state.Query) with
        | Some exprInfo -> this.RunSelectExprMapped(state.Query, exprInfo, Seq.tryHead)
        | None -> this.RunMapped(state.Query, Seq.tryHead)

    // Run: head - 'Selected
    member this.Run(state: QuerySource<ResultModifier.Head<'Selected>, Query>) =
        match SelectExprVisitors.SelectExprStore.tryGet(state.Query) with
        | Some exprInfo -> this.RunSelectExpr(state.Query, exprInfo, Seq.head)
        | None -> this.RunSelected(state.Query, Seq.head)

    // Run: head - 'Mapped
    member this.Run(state: QuerySource<ResultModifier.Head<'Mapped>, Query>) =
        match SelectExprVisitors.SelectExprStore.tryGet(state.Query) with
        | Some exprInfo -> this.RunSelectExprMapped(state.Query, exprInfo, Seq.head)
        | None -> this.RunMapped(state.Query, Seq.head)

    // Run: count
    member this.Run(state: QuerySource<ResultModifier.Count<int>, Query>) =
        task {
            let! ctx = ContextUtils.getContext ct
            try return! ctx.CountAsyncWithOptions (SelectQuery<int>(state.Query), this.CancellationToken) |> Async.AwaitTask
            finally ContextUtils.disposeIfNotShared ct ctx
        }


/// A select builder that returns an Async result.
type SelectAsyncBuilder<'Selected, 'Mapped> (ct: ContextType) =
    inherit SelectBuilder<'Selected, 'Mapped>()

    member this.RunSelected(query: Query, resultModifier) =
        async {
            let! ctx = ContextUtils.getContext ct |> Async.AwaitTask
            try
                use cmd = ctx.BuildCommand(query)
                let! asyncCancel = Async.CancellationToken
                let cancel = if this.CancellationToken <> CancellationToken.None then this.CancellationToken else asyncCancel
                use! reader = cmd.ExecuteReaderAsync(cancel) |> Async.AwaitTask
                let readEntity = Hydration.buildRowReader<'Selected> ctx.Provider (reader : DbDataReader)
                let results = ResizeArray<'Selected>()

                let! hasMore = reader.ReadAsync(cancel) |> Async.AwaitTask
                let mutable hasMore = hasMore
                while hasMore && not cancel.IsCancellationRequested do
                    results.Add(readEntity())
                    let! hasMore' = reader.ReadAsync(cancel) |> Async.AwaitTask
                    hasMore <- hasMore'

                return results :> seq<'Selected> |> resultModifier
            finally
                ContextUtils.disposeIfNotShared ct ctx
        }

    member this.RunMapped(query: Query, resultModifier) =
        async {
            let! ctx = ContextUtils.getContext ct |> Async.AwaitTask
            try
                use cmd = ctx.BuildCommand(query)
                let! asyncCancel = Async.CancellationToken
                let cancel = if this.CancellationToken <> CancellationToken.None then this.CancellationToken else asyncCancel
                use! reader = cmd.ExecuteReaderAsync(cancel) |> Async.AwaitTask
                let readEntity = Hydration.buildRowReader<'Selected> ctx.Provider (reader : DbDataReader)
                let results = ResizeArray<'Mapped>()

                let! hasMore = reader.ReadAsync(cancel) |> Async.AwaitTask
                let mutable hasMore = hasMore
                while hasMore && not cancel.IsCancellationRequested do
                    results.Add(this.MapFn.Value.Invoke(readEntity()))
                    let! hasMore' = reader.ReadAsync(cancel) |> Async.AwaitTask
                    hasMore <- hasMore'

                return results :> seq<'Mapped> |> resultModifier
            finally
                ContextUtils.disposeIfNotShared ct ctx
        }

    member private this.RunSelectExpr(query: Query, exprInfo: SelectExprVisitors.SelectExprInfo, resultModifier) =
        async {
            let! ctx = ContextUtils.getContext ct |> Async.AwaitTask
            try
                use cmd = ctx.BuildCommand(query)
                let! asyncCancel = Async.CancellationToken
                let cancel = if this.CancellationToken <> CancellationToken.None then this.CancellationToken else asyncCancel
                use! reader = cmd.ExecuteReaderAsync(cancel) |> Async.AwaitTask
                let readRow = Hydration.buildSelectExprReader ctx.Provider (reader : DbDataReader) exprInfo

                let results = ResizeArray<'Selected>()
                let! hasMore = reader.ReadAsync(cancel) |> Async.AwaitTask
                let mutable hasMore = hasMore
                while hasMore do
                    let fields = readRow()
                    let result = exprInfo.CompiledMapper.Invoke(fields) :?> 'Selected
                    results.Add(result)
                    let! hasMore' = reader.ReadAsync(cancel) |> Async.AwaitTask
                    hasMore <- hasMore'

                return results :> seq<'Selected> |> resultModifier
            finally
                ContextUtils.disposeIfNotShared ct ctx
        }

    member private this.RunSelectExprMapped(query: Query, exprInfo: SelectExprVisitors.SelectExprInfo, resultModifier) =
        async {
            let! ctx = ContextUtils.getContext ct |> Async.AwaitTask
            try
                use cmd = ctx.BuildCommand(query)
                let! asyncCancel = Async.CancellationToken
                let cancel = if this.CancellationToken <> CancellationToken.None then this.CancellationToken else asyncCancel
                use! reader = cmd.ExecuteReaderAsync(cancel) |> Async.AwaitTask
                let readRow = Hydration.buildSelectExprReader ctx.Provider (reader : DbDataReader) exprInfo

                let results = ResizeArray<'Mapped>()
                let! hasMore = reader.ReadAsync(cancel) |> Async.AwaitTask
                let mutable hasMore = hasMore
                while hasMore do
                    let fields = readRow()
                    let selected = exprInfo.CompiledMapper.Invoke(fields) :?> 'Selected
                    results.Add(this.MapFn.Value.Invoke(selected))
                    let! hasMore' = reader.ReadAsync(cancel) |> Async.AwaitTask
                    hasMore <- hasMore'

                return results :> seq<'Mapped> |> resultModifier
            finally
                ContextUtils.disposeIfNotShared ct ctx
        }

    /// Run: default
    /// Called when no mapSeq, mapArray or mapList is present;
    /// this input will always be 'Selected -- even if select is not present.
    member this.Run(state: QuerySource<'Selected, Query>) =
        match SelectExprVisitors.SelectExprStore.tryGet(state.Query) with
        | Some exprInfo -> this.RunSelectExpr(state.Query, exprInfo, id)
        | None -> this.RunSelected(state.Query, id)

    /// Run: toList
    member this.Run(state: QuerySource<'Selected list, Query>) =
        match SelectExprVisitors.SelectExprStore.tryGet(state.Query) with
        | Some exprInfo -> this.RunSelectExpr(state.Query, exprInfo, Seq.toList)
        | None -> this.RunSelected(state.Query, Seq.toList)

    /// Run: toArray
    member this.Run(state: QuerySource<'Selected array, Query>) =
        match SelectExprVisitors.SelectExprStore.tryGet(state.Query) with
        | Some exprInfo -> this.RunSelectExpr(state.Query, exprInfo, Seq.toArray)
        | None -> this.RunSelected(state.Query, Seq.toArray)

    /// Run: mapList
    member this.Run(state: QuerySource<'Mapped list, Query>) =
        match SelectExprVisitors.SelectExprStore.tryGet(state.Query) with
        | Some exprInfo -> this.RunSelectExprMapped(state.Query, exprInfo, Seq.toList)
        | None -> this.RunMapped(state.Query, Seq.toList)

    // Run: mapArray
    member this.Run(state: QuerySource<'Mapped array, Query>) =
        match SelectExprVisitors.SelectExprStore.tryGet(state.Query) with
        | Some exprInfo -> this.RunSelectExprMapped(state.Query, exprInfo, Seq.toArray)
        | None -> this.RunMapped(state.Query, Seq.toArray)

    // Run: mapSeq
    member this.Run(state: QuerySource<'Mapped seq, Query>) =
        match SelectExprVisitors.SelectExprStore.tryGet(state.Query) with
        | Some exprInfo -> this.RunSelectExprMapped(state.Query, exprInfo, id)
        | None -> this.RunMapped(state.Query, id)

    // Run: tryHead - 'Selected
    member this.Run(state: QuerySource<'Selected option, Query>) =
        match SelectExprVisitors.SelectExprStore.tryGet(state.Query) with
        | Some exprInfo -> this.RunSelectExpr(state.Query, exprInfo, Seq.tryHead)
        | None -> this.RunSelected(state.Query, Seq.tryHead)

    // Run: tryHead - 'Mapped
    member this.Run(state: QuerySource<'Mapped option, Query>) =
        match SelectExprVisitors.SelectExprStore.tryGet(state.Query) with
        | Some exprInfo -> this.RunSelectExprMapped(state.Query, exprInfo, Seq.tryHead)
        | None -> this.RunMapped(state.Query, Seq.tryHead)

    // Run: head - 'Selected
    member this.Run(state: QuerySource<ResultModifier.Head<'Selected>, Query>) =
        match SelectExprVisitors.SelectExprStore.tryGet(state.Query) with
        | Some exprInfo -> this.RunSelectExpr(state.Query, exprInfo, Seq.head)
        | None -> this.RunSelected(state.Query, Seq.head)

    // Run: head - 'Mapped
    member this.Run(state: QuerySource<ResultModifier.Head<'Mapped>, Query>) =
        match SelectExprVisitors.SelectExprStore.tryGet(state.Query) with
        | Some exprInfo -> this.RunSelectExprMapped(state.Query, exprInfo, Seq.head)
        | None -> this.RunMapped(state.Query, Seq.head)

    // Run: count
    member this.Run(state: QuerySource<ResultModifier.Count<int>, Query>) =
        async {
            let! ctx = ContextUtils.getContext ct |> Async.AwaitTask
            let! asyncCancel = Async.CancellationToken
            let cancel = if this.CancellationToken <> CancellationToken.None then this.CancellationToken else asyncCancel
            try return! ctx.CountAsyncWithOptions (SelectQuery<int>(state.Query), cancel) |> Async.AwaitTask
            finally ContextUtils.disposeIfNotShared ct ctx
        }


/// Builds and returns a select query that can be manually run by piping into QueryContext read methods
let select<'Selected, 'Mapped> =
    SelectQueryBuilder<'Selected, 'Mapped>()

/// Builds a select query with a context source - returns an Async query result
let inline selectAsync< ^Selected, ^Mapped, ^Context
    when (ContextTypeResolver.Resolver or ^Context) : (static member ($) : ContextTypeResolver.Resolver * ^Context -> ContextType)>
    (ctSource: ^Context) =
    let ct = ContextTypeResolver.resolve ctSource
    SelectAsyncBuilder< ^Selected, ^Mapped>(ct)

/// Builds a select query with a context source - returns a Task query result
let inline selectTask< ^Selected, ^Mapped, ^Context
    when (ContextTypeResolver.Resolver or ^Context) : (static member ($) : ContextTypeResolver.Resolver * ^Context -> ContextType)>
    (ctSource: ^Context) =
    let ct = ContextTypeResolver.resolve ctSource
    SelectTaskBuilder< ^Selected, ^Mapped>(ct)


