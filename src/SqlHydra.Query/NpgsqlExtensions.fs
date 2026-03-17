module SqlHydra.Query.NpgsqlExtensions

open System
open SqlKata

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

    /// Performs an upsert with COALESCE for specified columns (preserves existing value when new value is NULL).
    [<CustomOperation("onConflictDoUpdateCoalesce", MaintainsVariableSpace = true)>]
    member this.OnConflictDoUpdateCoalesce(state: QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>,
        [<ProjectionParameter>] conflictFields,
        [<ProjectionParameter>] updateFields,
        [<ProjectionParameter>] coalesceFields) =

        let spec = state.Query
        let conflictFields = LinqExpressionVisitors.visitPropertiesSelector<'T, 'ConflictProperty> conflictFields (fun tblAlias p -> p.Name)
        let updateFields = LinqExpressionVisitors.visitPropertiesSelector<'T, 'UpdateProperties> updateFields (fun tblAlias p -> p.Name)
        let coalesceFields = LinqExpressionVisitors.visitPropertiesSelector<'T, 'CoalesceProperties> coalesceFields (fun tblAlias p -> p.Name)
        let newSpec = { spec with InsertType = OnConflictDoUpdateCoalesce (conflictFields, updateFields, coalesceFields) }
        QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(newSpec, state.TableMappings)

    /// Insert is ignored if a conflict occurs.
    [<CustomOperation("onConflictDoNothing", MaintainsVariableSpace = true)>]
    member this.OnConflictDoNothing(state: QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>,
        [<ProjectionParameter>] conflictFields) =

        let spec = state.Query
        let conflictFields = LinqExpressionVisitors.visitPropertiesSelector<'T, 'ConflictProperty> conflictFields (fun tblAlias p -> p.Name)
        let newSpec = { spec with InsertType = OnConflictDoNothing conflictFields }
        QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(newSpec, state.TableMappings)

    /// Insert is ignored if a conflict occurs, with a WHERE clause for partial index targeting.
    [<CustomOperation("onConflictDoNothingWhere", MaintainsVariableSpace = true)>]
    member this.OnConflictDoNothingWhere(state: QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>,
        [<ProjectionParameter>] conflictFields,
        whereClause: string) =

        let spec = state.Query
        let conflictFields = LinqExpressionVisitors.visitPropertiesSelector<'T, 'ConflictProperty> conflictFields (fun tblAlias p -> p.Name)
        let newSpec = { spec with InsertType = OnConflictDoNothingWhere (conflictFields, whereClause) }
        QuerySource<'T, InsertQuerySpec<'T, 'InsertReturn>>(newSpec, state.TableMappings)

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

    /// ORDER BY column NULLS LAST
    [<CustomOperation("orderByNullsLast", MaintainsVariableSpace = true)>]
    member this.OrderByNullsLast (state: QuerySource<'T, Query>, [<ProjectionParameter>] propertySelector) =
        let result = LinqExpressionVisitors.visitOrderByPropertySelector<'T, 'Prop> propertySelector
        let orderedQuery =
            match result with
            | LinqExpressionVisitors.OrderByColumn (tableAlias, p) ->
                let fqCol = $"\"{tableAlias}\".\"{p.Name}\""
                state.Query.OrderByRaw($"{fqCol} NULLS LAST")
            | LinqExpressionVisitors.OrderByIgnored -> state.Query
            | _ -> failwith "NULLS LAST does not support aggregate columns"
        QuerySource<'T, Query>(orderedQuery, state.TableMappings)

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

    /// ORDER BY column DESC NULLS FIRST
    [<CustomOperation("orderByDescNullsFirst", MaintainsVariableSpace = true)>]
    member this.OrderByDescNullsFirst (state: QuerySource<'T, Query>, [<ProjectionParameter>] propertySelector) =
        let result = LinqExpressionVisitors.visitOrderByPropertySelector<'T, 'Prop> propertySelector
        let orderedQuery =
            match result with
            | LinqExpressionVisitors.OrderByColumn (tableAlias, p) ->
                let fqCol = $"\"{tableAlias}\".\"{p.Name}\""
                state.Query.OrderByRaw($"{fqCol} DESC NULLS FIRST")
            | LinqExpressionVisitors.OrderByIgnored -> state.Query
            | _ -> failwith "NULLS FIRST does not support aggregate columns"
        QuerySource<'T, Query>(orderedQuery, state.TableMappings)

