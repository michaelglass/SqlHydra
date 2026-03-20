/// Linq insert query builders
[<AutoOpen>]
module SqlHydra.Query.InsertBuilders

open System
open System.Threading

/// The base insert builder that contains all common operations
type InsertBuilder<'Inserted, 'InsertReturn>() =

    let getQueryOrDefault (state: QuerySource<'T>) =
        match state with
        | :? QuerySource<'T, InsertQuerySpec<'T, 'IdentityReturn>> as qs -> qs.Query
        | _ -> InsertQuerySpec.Default

    member val CancellationToken = CancellationToken.None with get, set

    member this.For (state: QuerySource<'T>, [<ReflectedDefinition>] forExpr: FSharp.Quotations.Expr<'T -> QuerySource<'T>>) =        
        let query = state |> getQueryOrDefault
        let tableAlias = QuotationVisitor.visitFor forExpr |> QuotationVisitor.allowUnderscore false
        let tblMaybe, tableMappings = TableMappings.tryGetByRootOrAlias tableAlias state.TableMappings
        let tbl = tblMaybe |> Option.get

        QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(
            { query with Table = $"{tbl.Schema}.{tbl.Name}" }
            , tableMappings)

    /// Sets the TABLE name for query.
    [<CustomOperation("into")>]
    member this.Into (state: QuerySource<'T>, table: QuerySource<'T>) =
        let tbl = TableMappings.getFirst table.TableMappings
        let query = state |> getQueryOrDefault
        QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(
            { query with Table = $"{tbl.Schema}.{tbl.Name}" }
            , state.TableMappings)

    member this.Yield _ =
        QuerySource<'T>(Map.empty)

    /// Sets a single value for INSERT
    [<CustomOperation("entity", MaintainsVariableSpace = true)>]
    member this.Entity (state:QuerySource<'T>, value: 'T) = 
        let spec = state |> getQueryOrDefault
        QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(
            { spec with Entities = [ value ] }
            , state.TableMappings)

    /// Sets multiple values for INSERT. (Must have at least one value.)
    [<CustomOperation("entities", MaintainsVariableSpace = true)>]
    member this.Entities (state:QuerySource<'T>, entities: AtLeastOne.AtLeastOne<'T>) = 
        let spec = state |> getQueryOrDefault
        QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(
            { spec with Entities = entities |> AtLeastOne.getSeq |> Seq.toList }
            , state.TableMappings)

    /// Sets multiple values for INSERT. (Should have at least one value.)
    [<CustomOperation("entities", MaintainsVariableSpace = true)>]
    member this.Entities (state:QuerySource<'T>, entities: 'T seq) = 
        let spec = state |> getQueryOrDefault
        QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(
            { spec with Entities = entities |> Seq.toList }
            , state.TableMappings)

    /// Includes a column in the insert query.
    [<CustomOperation("includeColumn", MaintainsVariableSpace = true)>]
    member this.IncludeColumn (state: QuerySource<'T>, [<ProjectionParameter>] propertySelector) = 
        let spec = state |> getQueryOrDefault
        let prop = (propertySelector |> LinqExpressionVisitors.visitPropertySelector<'T, 'Prop>).Name
        QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>({ spec with Fields = spec.Fields @ [ prop ] }, state.TableMappings)

    /// Excludes a column from the insert query.
    [<CustomOperation("excludeColumn", MaintainsVariableSpace = true)>]
    member this.ExcludeColumn (state: QuerySource<'T>, [<ProjectionParameter>] propertySelector) = 
        let spec = state |> getQueryOrDefault
        let prop = LinqExpressionVisitors.visitPropertySelector<'T, 'Prop> propertySelector
        let newSpec =
            spec.Fields
            |> function
                | [] -> FSharp.Reflection.FSharpType.GetRecordFields(typeof<'T>) |> Array.map (fun x -> x.Name) |> Array.toList
                | fields -> fields
            |> List.filter (fun f -> f <> prop.Name)
            |> (fun x -> { spec with Fields = x })
        QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(newSpec, state.TableMappings)
    
    /// Sets the identity field that should be returned from the insert and excludes it from the insert columns.
    [<CustomOperation("getId", MaintainsVariableSpace = true)>]
    member this.GetId (state: QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>, [<ProjectionParameter>] idProperty) = 
        // Exclude the identity column
        let spec = this.ExcludeColumn(state, idProperty).Query
        let prop = LinqExpressionVisitors.visitPropertySelector<'T, 'InsertReturn> idProperty :?> Reflection.PropertyInfo
        
        QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>({ spec with IdentityField = Some prop.Name }, state.TableMappings)

    /// Inserts rows from a SELECT query.
    /// Note: Column alignment between SELECT and target table is validated at runtime by the database,
    /// not at compile time. Ensure the SELECT columns match the target table's column order and types.
    [<CustomOperation("fromSelect", MaintainsVariableSpace = true)>]
    member this.FromSelect (state: QuerySource<'T>, selectQuery: SelectQuery<'Selected>) =
        let spec = state |> getQueryOrDefault
        let newSpec = { spec with InsertType = InsertFromSelect (selectQuery.ToKataQuery()) }
        QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(newSpec, state.TableMappings)

    /// Returns specified columns from the inserted row (PostgreSQL RETURNING, SQLite RETURNING, MariaDB RETURNING).
    /// SQL Server uses the separate `output` operation instead.
    [<CustomOperation("returning", MaintainsVariableSpace = true)>]
    member this.Returning (state: QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>, [<ProjectionParameter>] selectExpression) =
        let spec = state.Query
        let selections = LinqExpressionVisitors.visitSelect<'T,'InsertReturn> selectExpression
        let newSpec =
            selections
            |> List.choose (function
                | LinqExpressionVisitors.SelectedColumn (tableAlias, column, columnType, isOpt, isNullable) ->
                    Some (tableAlias, column, columnType, isOpt, isNullable)
                | _ ->
                    None
            )
            |> List.fold (fun (spec: InsertQuerySpec<'T, 'InsertReturn>) (_, column, propertyType, isOptional, isNullable) ->
                let nullability = if isOptional then IsOptional elif isNullable then IsNullable else NotNullable
                let outputField = { ColumnName = column; PropertyType = propertyType; Nullability = nullability }
                { spec with OutputFields = spec.OutputFields @ [outputField ] }
            ) spec
        QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(newSpec, state.TableMappings)

    /// Sets the conflict target to typed column(s).
    [<CustomOperation("onConflict", MaintainsVariableSpace = true)>]
    member this.OnConflict(state: QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>,
        [<ProjectionParameter>] conflictFields) =
        let spec = state.Query
        let conflictFields = LinqExpressionVisitors.visitPropertiesSelector<'T, 'ConflictProperty> conflictFields (fun tblAlias p -> p.Name)
        let newSpec = { spec with ConflictTarget = Some (TypedColumns conflictFields) }
        QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(newSpec, state.TableMappings)

    /// Conflict action: DO NOTHING.
    [<CustomOperation("doNothing", MaintainsVariableSpace = true)>]
    member this.DoNothing(state: QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>) =
        let spec = state.Query
        match spec.ConflictTarget with
        | Some target ->
            let newSpec = { spec with InsertType = OnConflict (target, DoNothing); ConflictTarget = None }
            QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(newSpec, state.TableMappings)
        | None -> failwith "doNothing requires onConflict to be called first"

    /// Conflict action: DO UPDATE SET col=EXCLUDED.col for each update field.
    [<CustomOperation("doUpdate", MaintainsVariableSpace = true)>]
    member this.DoUpdate(state: QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>,
        [<ProjectionParameter>] updateFields) =
        let spec = state.Query
        let updateFields = LinqExpressionVisitors.visitPropertiesSelector<'T, 'UpdateProperties> updateFields (fun tblAlias p -> p.Name)
        match spec.ConflictTarget with
        | Some target ->
            let newSpec = { spec with InsertType = OnConflict (target, DoUpdate updateFields); ConflictTarget = None }
            QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(newSpec, state.TableMappings)
        | None -> failwith "doUpdate requires onConflict to be called first"

    /// Conflict action: DO UPDATE SET with COALESCE for specified columns.
    /// Generates: SET col = COALESCE(EXCLUDED."col", "table"."col") — preserves existing value when new is NULL.
    [<CustomOperation("doUpdateCoalesce", MaintainsVariableSpace = true)>]
    member this.DoUpdateCoalesce(state: QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>,
        [<ProjectionParameter>] updateFields,
        [<ProjectionParameter>] coalesceFields) =
        let spec = state.Query
        let updateFields = LinqExpressionVisitors.visitPropertiesSelector<'T, 'UpdateProperties> updateFields (fun tblAlias p -> p.Name)
        let coalesceFields = LinqExpressionVisitors.visitPropertiesSelector<'T, 'CoalesceProperties> coalesceFields (fun tblAlias p -> p.Name)
        match spec.ConflictTarget with
        | Some target ->
            let newSpec = { spec with InsertType = OnConflict (target, DoUpdateCoalesce (updateFields, coalesceFields)); ConflictTarget = None }
            QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(newSpec, state.TableMappings)
        | None -> failwith "doUpdateCoalesce requires onConflict to be called first"

    /// Sets a CancellationToken for the query execution.
    [<CustomOperation("cancel", MaintainsVariableSpace = true)>]
    member this.Cancel (state: QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>, cancellationToken: CancellationToken) =
        this.CancellationToken <- cancellationToken
        state

    member this.Run (state: QuerySource<'Inserted>) =
        let spec = getQueryOrDefault state
        InsertQuery<'Inserted, 'InsertReturn>(spec)


/// An insert builder that returns a Task result.
type InsertAsyncBuilder<'Inserted, 'InsertReturn>(ct: ContextType) =
    inherit InsertBuilder<'Inserted, 'InsertReturn>()

    member this.Run (state: QuerySource<'Inserted, InsertQuerySpec<'Inserted, 'InsertReturn>>) = 
        async {
            let! ctx = ContextUtils.getContext ct |> Async.AwaitTask 
            try 
                let insertQuery = InsertQuery<'Inserted, 'InsertReturn>(state.Query)
                let! asyncCancel = Async.CancellationToken
                let cancel = if this.CancellationToken <> CancellationToken.None then this.CancellationToken else asyncCancel
                if state.Query.Entities |> Seq.isEmpty && (match state.Query.InsertType with InsertFromSelect _ -> false | _ -> true) then
                    return Unchecked.defaultof<'InsertReturn>
                else
                    let! insertReturn = ctx.InsertAsyncWithOptions (insertQuery, cancel) |> Async.AwaitTask
                    return insertReturn
            finally 
                ContextUtils.disposeIfNotShared ct ctx
        }


/// An insert builder that returns an Async result.
type InsertTaskBuilder<'Inserted, 'InsertReturn>(ct: ContextType) =
    inherit InsertBuilder<'Inserted, 'InsertReturn>()

    member this.Run (state: QuerySource<'Inserted, InsertQuerySpec<'Inserted, 'InsertReturn>>) =
        task {
            let! ctx = ContextUtils.getContext ct
            try
                let insertQuery = InsertQuery<'Inserted, 'InsertReturn>(state.Query)
                if state.Query.Entities |> Seq.isEmpty && (match state.Query.InsertType with InsertFromSelect _ -> false | _ -> true) then
                    return Unchecked.defaultof<'InsertReturn>
                else
                    let! insertReturn = ctx.InsertAsyncWithOptions (insertQuery, this.CancellationToken)
                    return insertReturn
            finally
                ContextUtils.disposeIfNotShared ct ctx
        }


/// Builds an insert query that can be manually run by piping into QueryContext insert methods
let insert<'Inserted, 'InsertReturn> = 
    InsertBuilder<'Inserted, 'InsertReturn>()

/// Builds an insert query that returns an Async result
let inline insertAsync< ^Inserted, ^InsertReturn, ^Context
    when (ContextTypeResolver.Resolver or ^Context) : (static member ($) : ContextTypeResolver.Resolver * ^Context -> ContextType)>
    (ctSource: ^Context) =
    let ct = ContextTypeResolver.resolve ctSource
    InsertAsyncBuilder< ^Inserted, ^InsertReturn>(ct)

/// Builds an insert query that returns a Task result
let inline insertTask< ^Inserted, ^InsertReturn, ^Context
    when (ContextTypeResolver.Resolver or ^Context) : (static member ($) : ContextTypeResolver.Resolver * ^Context -> ContextType)>
    (ctSource: ^Context) =
    let ct = ContextTypeResolver.resolve ctSource
    InsertTaskBuilder< ^Inserted, ^InsertReturn>(ct)
    
