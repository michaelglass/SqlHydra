module SqlHydra.Query.NpgsqlExtensions

open System

/// Common PostgreSQL functions for use in select expressions.
/// Use `open type SqlFn` to access functions without qualification.
type SqlFn =
    // String functions (PostgreSQL uses lowercase)
    static member char_length(s: string) : int = sqlFn
    static member character_length(s: string) : int = sqlFn
    static member length(s: string) : int = sqlFn
    static member upper(s: string) : string = sqlFn
    static member lower(s: string) : string = sqlFn
    static member ltrim(s: string) : string = sqlFn
    static member rtrim(s: string) : string = sqlFn
    static member btrim(s: string) : string = sqlFn
    static member trim(s: string) : string = sqlFn
    static member substring(s: string, start: int, length: int) : string = sqlFn
    static member replace(s: string, from: string, ``to``: string) : string = sqlFn
    static member position(substring: string, s: string) : int = sqlFn
    static member strpos(s: string, substring: string) : int = sqlFn
    static member concat(s1: string, s2: string) : string = sqlFn
    static member concat(s1: string, s2: string, s3: string) : string = sqlFn
    static member concat_ws(separator: string, s1: string, s2: string) : string = sqlFn
    static member concat_ws(separator: string, s1: string, s2: string, s3: string) : string = sqlFn
    static member left(s: string, length: int) : string = sqlFn
    static member right(s: string, length: int) : string = sqlFn
    static member reverse(s: string) : string = sqlFn
    static member repeat(s: string, count: int) : string = sqlFn
    static member lpad(s: string, length: int, fill: string) : string = sqlFn
    static member rpad(s: string, length: int, fill: string) : string = sqlFn
    static member initcap(s: string) : string = sqlFn

    // Null handling - with overloads for Option and Nullable
    static member coalesce(a: Option<'T>, b: 'T) : 'T = sqlFn
    static member coalesce(a: Nullable<'T>, b: 'T) : 'T when 'T : struct = sqlFn
    static member coalesce(a: 'T, b: 'T) : 'T = sqlFn
    static member coalesce(a: 'T, b: 'T, c: 'T) : 'T = sqlFn
    static member nullif(a: 'T, b: 'T) : Option<'T> = sqlFn

    // Numeric functions
    static member abs(n: 'T) : 'T when 'T : struct = sqlFn
    static member round(n: 'T) : 'T when 'T : struct = sqlFn
    static member round(n: 'T, decimals: int) : 'T when 'T : struct = sqlFn
    static member ceil(n: 'T) : 'T when 'T : struct = sqlFn
    static member ceiling(n: 'T) : 'T when 'T : struct = sqlFn
    static member floor(n: 'T) : 'T when 'T : struct = sqlFn
    static member sign(n: 'T) : int when 'T : struct = sqlFn
    static member power(n: 'T, exponent: 'T) : 'T when 'T : struct = sqlFn
    static member sqrt(n: 'T) : float when 'T : struct = sqlFn
    static member mod'(n: 'T, divisor: 'T) : 'T when 'T : struct = sqlFn
    static member trunc(n: 'T) : 'T when 'T : struct = sqlFn
    static member trunc(n: 'T, decimals: int) : 'T when 'T : struct = sqlFn

    // Date/time functions
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

type InsertBuilder<'Inserted, 'InsertReturn> with
    
    /// Performs an update on one or more update fields if a conflict occurs.
    [<CustomOperation("onConflictDoUpdate", MaintainsVariableSpace = true)>]
    member this.OnConflictDoUpdate(state: QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>, 
        [<ProjectionParameter>] conflictFields, 
        [<ProjectionParameter>] updateFields) = 
        
        let spec = state.Query
        let conflictFields = LinqExpressionVisitors.visitPropertiesSelector<'T, 'ConflictProperty> conflictFields (fun tblAlias p -> p.Name)
        let updateFields = LinqExpressionVisitors.visitPropertiesSelector<'T, 'UpdateProperties> updateFields (fun tblAlias p -> p.Name)
        let newSpec = { spec with InsertType = OnConflictDoUpdate (conflictFields, updateFields) }
        QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(newSpec, state.TableMappings)

    /// Insert is ignored if a conflict occurs.
    [<CustomOperation("onConflictDoNothing", MaintainsVariableSpace = true)>]
    member this.OnConflictDoNothing(state: QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>,
        [<ProjectionParameter>] conflictFields) =

        let spec = state.Query
        let conflictFields = LinqExpressionVisitors.visitPropertiesSelector<'T, 'ConflictProperty> conflictFields (fun tblAlias p -> p.Name)
        let newSpec = { spec with InsertType = OnConflictDoNothing conflictFields }
        QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(newSpec, state.TableMappings)

    /// ON CONFLICT (cols) DO UPDATE SET col = COALESCE(EXCLUDED.col, table.col).
    /// Preserves existing non-null values rather than overwriting with NULL.
    [<CustomOperation("onConflictDoUpdateCoalesce", MaintainsVariableSpace = true)>]
    member this.OnConflictDoUpdateCoalesce(state: QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>,
        [<ProjectionParameter>] conflictFields,
        [<ProjectionParameter>] updateFields) =
        let spec = state.Query
        let conflictFields = LinqExpressionVisitors.visitPropertiesSelector<'T, 'ConflictProperty> conflictFields (fun _ p -> p.Name)
        let updateFields = LinqExpressionVisitors.visitPropertiesSelector<'T, 'UpdateProperties> updateFields (fun _ p -> p.Name)
        let newSpec = { spec with InsertType = OnConflictDoUpdateCoalesce (conflictFields, updateFields) }
        QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(newSpec, state.TableMappings)

    /// ON CONFLICT (cols) WHERE <whereFragment> DO NOTHING — for partial-index conflicts.
    /// Use ? as parameter placeholders in the where fragment.
    [<CustomOperation("onConflictDoNothingWhereRaw", MaintainsVariableSpace = true)>]
    member this.OnConflictDoNothingWhereRaw(state: QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>,
        [<ProjectionParameter>] conflictFields,
        whereFragment: string,
        parameters: obj[]) =
        let spec = state.Query
        let conflictFields = LinqExpressionVisitors.visitPropertiesSelector<'T, 'ConflictProperty> conflictFields (fun _ p -> p.Name)
        let newSpec = { spec with InsertType = OnConflictDoNothingWhereRaw (conflictFields, whereFragment, parameters) }
        QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(newSpec, state.TableMappings)

    /// ON CONFLICT (<rawTargetExpr>) DO NOTHING — for expression indexes (e.g., lower(email)).
    [<CustomOperation("onConflictDoNothingRawTarget", MaintainsVariableSpace = true)>]
    member this.OnConflictDoNothingRawTarget(state: QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>,
        rawTargetExpr: string) =
        let spec = state.Query
        let newSpec = { spec with InsertType = OnConflictDoNothingRawTarget rawTargetExpr }
        QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(newSpec, state.TableMappings)

type SelectBuilder<'Selected, 'Mapped> with

    /// Adds DISTINCT ON (col) — PostgreSQL-only. Mutually exclusive with `distinct`.
    /// Multiple `distinctOn` calls accumulate columns.
    [<CustomOperation("distinctOn", MaintainsVariableSpace = true)>]
    member this.DistinctOn (state: QuerySource<'T, SelectQueryIR>, [<ProjectionParameter>] propertySelector: System.Linq.Expressions.Expression<Func<'T, 'Prop>>) =
        let ir = state.Query
        let column =
            LinqExpressionVisitors.visitOrderByPropertySelector<'T, 'Prop> propertySelector
            |> function
                | LinqExpressionVisitors.OrderByColumn (alias, p) -> $"{alias}.{p.Name}"
                | _ -> failwith "distinctOn requires a simple column selector."
        QuerySource<'T, SelectQueryIR>({ ir with DistinctOn = ir.DistinctOn @ [column] }, state.TableMappings)

    /// LEFT JOIN LATERAL (subquery) AS alias ON true. PostgreSQL-only.
    /// The subquery is built with its own select { ... } and may correlate to outer columns.
    [<CustomOperation("lateralJoin", MaintainsVariableSpace = true)>]
    member this.LateralJoin (state: QuerySource<'T, SelectQueryIR>, subquery: SelectQuery, alias: string) =
        let ir = state.Query
        let joinClause =
            { Kind = LeftJoinLateral
              Table = alias
              Subquery = Some subquery.SelectIR
              Condition = WhereClause.RawWhere("true", [||]) }
        QuerySource<'T, SelectQueryIR>({ ir with Joins = ir.Joins @ [joinClause] }, state.TableMappings)

