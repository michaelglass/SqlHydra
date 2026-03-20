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

/// PostgreSQL-specific extensions for the select builder.
type SelectBuilder<'Selected, 'Mapped> with

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


