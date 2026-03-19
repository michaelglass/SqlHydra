module SqlHydra.Query.NpgsqlExtensions

open System
open SqlKata

/// PostgreSQL-specific SQL functions.
/// Use `open type PgSqlFn` for PostgreSQL-only functions.
/// Standard functions (coalesce, abs, round, etc.) are in SqlFn.
type PgSqlFn =
    // PostgreSQL-specific string functions
    static member char_length(s: string) : int = sqlFn
    static member character_length(s: string) : int = sqlFn
    static member length(s: string) : int = sqlFn
    static member ltrim(s: string) : string = sqlFn
    static member rtrim(s: string) : string = sqlFn
    static member btrim(s: string) : string = sqlFn
    static member position(substring: string, s: string) : int = sqlFn
    static member strpos(s: string, substring: string) : int = sqlFn
    static member concat_ws(separator: string, s1: string, s2: string) : string = sqlFn
    static member concat_ws(separator: string, s1: string, s2: string, s3: string) : string = sqlFn
    static member left(s: string, length: int) : string = sqlFn
    static member right(s: string, length: int) : string = sqlFn
    static member reverse(s: string) : string = sqlFn
    static member repeat(s: string, count: int) : string = sqlFn
    static member lpad(s: string, length: int, fill: string) : string = sqlFn
    static member rpad(s: string, length: int, fill: string) : string = sqlFn
    static member initcap(s: string) : string = sqlFn

    // pgvector distance functions (emit infix operators via visitSqlFn)
    // Second argument is typically inlineValue with a vector parameter
    static member cosine_distance(a: 'T, b: 'U) : float = sqlFn
    static member l2_distance(a: 'T, b: 'U) : float = sqlFn
    static member inner_product_distance(a: 'T, b: 'U) : float = sqlFn

    // Date/time functions (PostgreSQL-specific)
    static member now() : DateTime = sqlFn
    static member current_date() : DateTime = sqlFn
    static member current_time() : TimeSpan = sqlFn
    static member current_timestamp() : DateTime = sqlFn
    static member date_trunc(field: string, source: DateTime) : DateTime = sqlFn
    static member date_part(field: string, source: DateTime) : float = sqlFn
    static member extract(field: string, source: DateTime) : float = sqlFn
    static member age(timestamp: DateTime) : TimeSpan = sqlFn
    static member age(timestamp1: DateTime, timestamp2: DateTime) : TimeSpan = sqlFn
    static member make_date(year: int, month: int, day: int) : DateTime = sqlFn
    static member make_time(hour: int, minute: int, second: float) : TimeSpan = sqlFn

    // Interval (emits INTERVAL 'value' — special handling in visitSqlFn)
    static member interval(value: string) : TimeSpan = sqlFn

type InsertBuilder<'Inserted, 'InsertReturn> with

    /// Sets the conflict target to typed column(s).
    [<CustomOperation("onConflict", MaintainsVariableSpace = true)>]
    member this.OnConflict(state: QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>,
        [<ProjectionParameter>] conflictFields) =
        let spec = state.Query
        let conflictFields = LinqExpressionVisitors.visitPropertiesSelector<'T, 'ConflictProperty> conflictFields (fun tblAlias p -> p.Name)
        let newSpec = { spec with ConflictTarget = Some (TypedColumns conflictFields) }
        QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(newSpec, state.TableMappings)

    /// Sets the conflict target to a raw SQL expression (for expression indexes).
    [<CustomOperation("onConflictRaw", MaintainsVariableSpace = true)>]
    member this.OnConflictRaw(state: QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>,
        rawTarget: string) =
        let spec = state.Query
        let newSpec = { spec with ConflictTarget = Some (RawTarget rawTarget) }
        QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(newSpec, state.TableMappings)

    /// Adds a raw SQL WHERE clause to the conflict target (for partial unique indexes).
    [<CustomOperation("whereRawConflict", MaintainsVariableSpace = true)>]
    member this.WhereRawConflict(state: QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>,
        whereClause: string) =
        let spec = state.Query
        let newTarget =
            match spec.ConflictTarget with
            | Some (TypedColumns columns) -> TypedColumnsWhereRaw (columns, whereClause)
            | _ -> failwith "whereRawConflict requires onConflict to be called first with typed columns"
        let newSpec = { spec with ConflictTarget = Some newTarget }
        QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(newSpec, state.TableMappings)

    /// Conflict action: DO NOTHING.
    [<CustomOperation("doNothing", MaintainsVariableSpace = true)>]
    member this.DoNothing(state: QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>) =
        let spec = state.Query
        match spec.ConflictTarget with
        | Some target ->
            let newSpec = { spec with InsertType = OnConflict (target, DoNothing); ConflictTarget = None }
            QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(newSpec, state.TableMappings)
        | None -> failwith "doNothing requires onConflict or onConflictRaw to be called first"

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
        | None -> failwith "doUpdate requires onConflict or onConflictRaw to be called first"

    /// Conflict action: DO UPDATE SET with COALESCE for specified columns (PostgreSQL only).
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
        | None -> failwith "doUpdateCoalesce requires onConflict or onConflictRaw to be called first"

    /// Returns specified columns from the inserted row (PostgreSQL RETURNING clause).
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

/// PostgreSQL-specific extensions for the select builder.
type SelectBuilder<'Selected, 'Mapped> with

    /// Adds NULLS LAST to the most recent ORDER BY clause (PostgreSQL only)
    [<CustomOperation("nullsLast", MaintainsVariableSpace = true)>]
    member this.NullsLast (state: QuerySource<'T, Query>) =
        NullsStore.set state.Query "NULLS LAST"
        state

    /// Adds NULLS FIRST to the most recent ORDER BY clause (PostgreSQL only)
    [<CustomOperation("nullsFirst", MaintainsVariableSpace = true)>]
    member this.NullsFirst (state: QuerySource<'T, Query>) =
        NullsStore.set state.Query "NULLS FIRST"
        state

    /// SELECT DISTINCT ON (column) - returns first row per unique value of column (PostgreSQL only)
    [<CustomOperation("distinctOn", MaintainsVariableSpace = true)>]
    member this.DistinctOn (state: QuerySource<'T, Query>, [<ProjectionParameter>] propertySelector) =
        let result = LinqExpressionVisitors.visitOrderByPropertySelector<'T, 'Prop> propertySelector
        match result with
        | LinqExpressionVisitors.OrderByColumn (tableAlias, p) ->
            let fqCol = $"\"{tableAlias}\".\"{p.Name}\""
            let existing = DistinctOnStore.tryTake state.Query |> Option.defaultValue []
            DistinctOnStore.set state.Query (existing @ [fqCol])
        | _ -> ()
        state

    /// LEFT JOIN LATERAL (subquery) AS alias ON true (PostgreSQL only).
    /// The subquery typically correlates with the outer query via WhereRaw in kata.
    [<CustomOperation("lateralJoin", MaintainsVariableSpace = true)>]
    member this.LateralJoin (state: QuerySource<'T, Query>, innerQuery: SelectQuery, alias: string) =
        let subquery = innerQuery.ToKataQuery().Clone()
        subquery.As(alias) |> ignore
        let updatedQuery =
            state.Query.Join(
                subquery,
                (fun (j: SqlKata.Join) -> j.WhereRaw("true")),
                "left join lateral"
            )
        QuerySource<'T, Query>(updatedQuery, state.TableMappings)

    member private this.OrderByVectorDistance (state: QuerySource<'T, Query>, propertySelector, operator: string, vector: obj) =
        let result = LinqExpressionVisitors.visitOrderByPropertySelector<'T, 'Prop> propertySelector
        match result with
        | LinqExpressionVisitors.OrderByColumn (tableAlias, p) ->
            let fqCol = $"\"{tableAlias}\".\"{p.Name}\""
            QuerySource<'T, Query>(state.Query.OrderByRaw($"{fqCol} {operator} ?", [| vector |]), state.TableMappings)
        | LinqExpressionVisitors.OrderByIgnored -> state
        | _ -> failwith "pgvector distance ordering requires a column reference, not an aggregate"

    /// ORDER BY column <=> @vector (pgvector cosine distance, ascending — closest first)
    [<CustomOperation("orderByCosineDistance", MaintainsVariableSpace = true)>]
    member this.OrderByCosineDistance (state: QuerySource<'T, Query>, [<ProjectionParameter>] propertySelector, vector: obj) =
        this.OrderByVectorDistance(state, propertySelector, "<=>", vector)

    /// ORDER BY column <-> @vector (pgvector L2/Euclidean distance, ascending — closest first)
    [<CustomOperation("orderByL2Distance", MaintainsVariableSpace = true)>]
    member this.OrderByL2Distance (state: QuerySource<'T, Query>, [<ProjectionParameter>] propertySelector, vector: obj) =
        this.OrderByVectorDistance(state, propertySelector, "<->", vector)

    /// ORDER BY column <#> @vector (pgvector inner product distance, ascending)
    [<CustomOperation("orderByInnerProductDistance", MaintainsVariableSpace = true)>]
    member this.OrderByInnerProductDistance (state: QuerySource<'T, Query>, [<ProjectionParameter>] propertySelector, vector: obj) =
        this.OrderByVectorDistance(state, propertySelector, "<#>", vector)

