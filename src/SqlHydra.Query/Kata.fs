namespace SqlHydra.Query

open System.Reflection
open SqlKata
open System.Collections.Generic
open System

type TableMapping =
    {
        Name: string
        Schema: string
        RecordType: System.Type option
        CteQuery: SqlKata.Query option
    }
    member this.IsCte = this.Schema = ""
    member this.IsInTable (m: Linq.Expressions.MemberExpression) =
        match this.RecordType with
        | Some rt ->
            // CTE source: match by record type
            m.Member.ReflectedType = rt || m.Member.DeclaringType = rt
        | None ->
            // Regular table source: match by schema.table type hierarchy
            m.Member.ReflectedType.DeclaringType <> null &&
            m.Member.ReflectedType.DeclaringType.Name = this.Schema &&
            m.Member.ReflectedType.Name = this.Name

type TableMappingKey = 
    | Root
    | TableAliasKey of string
        
module TableMappings = 

    /// Tries to get TableMapping by Root, then by Alias.
    /// If found by Root, replaces with a TableAliasKey.
    let tryGetByRootOrAlias (tableAlias: string) (tableMappings: Map<TableMappingKey, TableMapping>) =
        match tableMappings.TryFind(Root) with
        | Some tbl -> 
            let updatedTableMappings = tableMappings.Remove(Root).Add(TableAliasKey tableAlias, tbl)  
            Some tbl, updatedTableMappings
        | None -> 
            match tableMappings.TryFind(TableAliasKey tableAlias) with
            | Some tbl -> Some tbl, tableMappings
            | None -> None, tableMappings

    /// Gets the first TableMapping.
    let getFirst (tableMappings: Map<TableMappingKey, TableMapping>) = 
        tableMappings |> Map.toList |> List.map snd |> List.head

module FQ = 

    /// Fully qualifies a column with: {?schema}.{table}.{column}
    let internal fullyQualifyColumn (tables: Map<TableMappingKey, TableMapping>) (tableAlias: string) (column: Reflection.MemberInfo) =
        let tbl = tables[TableAliasKey tableAlias]
        $"%s{tbl.Schema}.%s{tbl.Name}.%s{column.Name}"

/// Represents a collection that must contain at least on item.
module AtLeastOne =
    type AtLeastOne<'T> = private { Items : 'T seq }

    /// Returns Some if seq contains at least one item, else returns None.
    let tryCreate<'T> (items: 'T seq) = 
        if items |> Seq.length > 0
        then Some { Items = items }
        else None

    let getSeq { Items = atLeastOne } = 
        atLeastOne

/// Wraps a SqlKata query parameter to provide the generated ProviderDbType attribute value.
type QueryParameter = 
    {
        Value: obj
        ProviderDbType: string option
    }
    /// Provides a more compact representation of the QueryParameter when logging queries.
    override this.ToString() = 
        match this.ProviderDbType with
        | Some providerDbType -> $"%s{providerDbType}: {this.Value}"
        | None -> $"obj: {this.Value}"

/// Wraps a SqlResult to customize query logging.
type LoggedSqlResult(r: SqlResult) = 
    inherit SqlResult()
    override this.ToString() = 
        let sb = Text.StringBuilder()
        sb.AppendLine(r.Sql) |> ignore
        for kvp in r.NamedBindings do
            sb.AppendLine($"- {kvp.Key}: {kvp.Value}") |> ignore
        sb.ToString()

type InsertType =
    | Insert
    | InsertOrReplace
    | OnConflictDoUpdate of conflictFields: string list * updateFields: string list
    | OnConflictDoUpdateCoalesce of conflictFields: string list * updateFields: string list * coalesceFields: string list
    | OnConflictDoNothing of conflictFields: string list
    | OnConflictDoNothingWhereRaw of conflictFields: string list * whereClause: string
    | OnConflictDoNothingRawTarget of rawTarget: string
    | InsertOrUpdateOnUnique of keyFields: string list * updateFields: string list
    | InsertFromSelect of selectQuery: SqlKata.Query

type InsertQuerySpec<'T, 'Identity> =
    {
        Table: string
        Entities: 'T list
        Fields: string list
        IdentityField: string option
        OutputFields: OutputField list
        InsertType: InsertType
    }
    static member Default : InsertQuerySpec<'T, 'Identity> = 
        { Table = ""; Entities = []; Fields = []; IdentityField = None; OutputFields = []; InsertType = Insert }

and OutputField = 
    {
        ColumnName: string
        PropertyType: Type
        Nullability: Nullability
    }

and Nullability = 
    | IsOptional
    | IsNullable
    | NotNullable

type UpdateQuerySpec<'T, 'UpdateReturn> =
    {
        Table: string
        Entity: 'T option
        Fields: string list
        SetValues: (string * obj) list
        SetRawValues: (string * string * obj array) list
        Where: Query option
        OutputFields: OutputField list
        UpdateAll: bool
    }
    static member Default : UpdateQuerySpec<'T, 'UpdateReturn> =
        { Table = ""; Entity = Option<'T>.None; Fields = []; SetValues = []; SetRawValues = []; Where = None; OutputFields = []; UpdateAll = false }

type QuerySource<'T>(tableMappings) =
    interface IEnumerable<'T> with
        member this.GetEnumerator() = Seq.empty<'T>.GetEnumerator() :> Collections.IEnumerator
        member this.GetEnumerator() = Seq.empty<'T>.GetEnumerator()
    member this.TableMappings : Map<TableMappingKey, TableMapping> = tableMappings

type QuerySource<'T, 'Query>(query, tableMappings) =
    inherit QuerySource<'T>(tableMappings)
    member this.Query : 'Query = query

/// The type of join for predicate-style joins
type JoinType =
    | Inner
    | Left

/// Information about a pending join that will be completed with an `on'` clause
type PendingJoin = {
    JoinType: JoinType
    TableName: string     // e.g., "Sales.SalesOrderDetail"
    TableAlias: string    // e.g., "d"
}

/// Module to store pending join info for queries using predicate-style joins.
/// This is a workaround for F# CE limitations that don't preserve subtype info.
module PendingJoins =
    open System.Runtime.CompilerServices

    // Use ConditionalWeakTable to associate PendingJoin with Query objects
    // without preventing garbage collection of the Query
    let private pendingJoins = ConditionalWeakTable<SqlKata.Query, PendingJoin>()

    /// Associates a pending join with a query
    let set (query: SqlKata.Query) (pendingJoin: PendingJoin) =
        // Remove any existing pending join first
        pendingJoins.Remove(query) |> ignore
        pendingJoins.Add(query, pendingJoin)

    /// Gets and removes the pending join for a query
    let tryTake (query: SqlKata.Query) =
        match pendingJoins.TryGetValue(query) with
        | true, pj ->
            pendingJoins.Remove(query) |> ignore
            Some pj
        | false, _ -> None

/// Module to store raw SET parameter bindings for update queries using setRaw.
module SetRawParamStore =
    open System.Runtime.CompilerServices

    let private store = ConditionalWeakTable<SqlKata.Query, (string * obj) list>()

    /// Associates raw SET parameter bindings with a query
    let set (query: SqlKata.Query) (parms: (string * obj) list) =
        store.Remove(query) |> ignore
        store.Add(query, parms)

    /// Gets raw SET parameter bindings for a query (non-destructive; entry cleaned up by GC).
    let tryGet (query: SqlKata.Query) =
        match store.TryGetValue(query) with
        | true, parms -> Some parms
        | false, _ -> None

/// Module to store DISTINCT ON column info for PostgreSQL queries.
module DistinctOnStore =
    open System.Runtime.CompilerServices

    let private store = ConditionalWeakTable<SqlKata.Query, string list>()

    /// Associates DISTINCT ON columns with a query
    let set (query: SqlKata.Query) (columns: string list) =
        store.Remove(query) |> ignore
        store.Add(query, columns)

    /// Gets and removes DISTINCT ON columns for a query
    let tryTake (query: SqlKata.Query) =
        match store.TryGetValue(query) with
        | true, cols ->
            store.Remove(query) |> ignore
            Some cols
        | false, _ -> None

    /// Applies DISTINCT ON to compiled SQL if present.
    /// Finds the first SELECT at parenthesis depth 0 (skipping CTEs and subqueries).
    let applyToSql (query: SqlKata.Query) (sql: string) =
        match tryTake query with
        | Some columns ->
            let distinctOnCsv = columns |> String.concat ", "
            let selectToken = "SELECT "
            let mutable depth = 0
            let mutable idx = -1
            for i in 0 .. sql.Length - 1 do
                if idx < 0 then
                    match sql.[i] with
                    | '(' -> depth <- depth + 1
                    | ')' -> depth <- depth - 1
                    | 'S' when depth = 0
                            && i + selectToken.Length <= sql.Length
                            && String.CompareOrdinal(sql, i, selectToken, 0, selectToken.Length) = 0 ->
                        idx <- i
                    | _ -> ()
            if idx >= 0 then
                sql.Insert(idx + selectToken.Length, $"DISTINCT ON ({distinctOnCsv}) ")
            else sql
        | None -> sql

module internal KataUtils =

    // Manually convert DateOnly to DateTime and TimeOnly to TimeSpan (until Microsoft.Data.SqlClient handles)
    let convertIfDateOnlyTimeOnly (value: obj) =
        match value with
#if NET6_0_OR_GREATER
        | :? DateOnly as dateOnly -> box (dateOnly.ToDateTime(TimeOnly.MinValue))
        | :? TimeOnly as timeOnly -> box (timeOnly.ToTimeSpan())
#endif
        | _ -> value

    /// Boxes values (and option values)
    let private boxValueOrOption (value: obj) = 
        if isNull value then 
            box System.DBNull.Value
        else
            match value.GetType() with
            | t when t.IsGenericType && t.Name.StartsWith("FSharpOption") -> 
                t.GetProperty("Value").GetValue(value)
            | t when t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Nullable<_>> -> 
                t.GetProperty("Value").GetValue(value)
            | _ -> value
            |> function 
                | null -> box System.DBNull.Value 
                | o -> o

    let private getProviderDbTypeName (p: MemberInfo) =
        match Attribute.GetCustomAttribute(p, typeof<SqlHydra.ProviderDbTypeAttribute>, false) with
        | :? SqlHydra.ProviderDbTypeAttribute as att -> Some att.ProviderDbTypeName
        | _ -> None

    let getQueryParameterForValue (p: MemberInfo) (value: obj) =
        { Value = value |> boxValueOrOption
        ; ProviderDbType = getProviderDbTypeName p } :> obj

    let getQueryParameterForEntity (entity: 'T) (p: PropertyInfo) =
        p.GetValue(entity) 
        |> getQueryParameterForValue p
        
    let fromUpdate (spec: UpdateQuerySpec<'T, 'UpdateReturn>) =
        let kvps =
            match spec.Entity, spec.SetValues with
            | Some entity, [] ->
                match spec.Fields with
                | [] ->
                    FSharp.Reflection.FSharpType.GetRecordFields(typeof<'T>)
                    |> Array.map (fun p -> p.Name, getQueryParameterForEntity entity p)

                | fields ->
                    let included = fields |> Set.ofList
                    FSharp.Reflection.FSharpType.GetRecordFields(typeof<'T>)
                    |> Array.filter (fun p -> included.Contains(p.Name))
                    |> Array.map (fun p -> p.Name, getQueryParameterForEntity entity p)

            | Some _, _ -> failwith "Cannot have both `entity` and `set` operations in an `update` expression."
            | None, [] when spec.SetRawValues.Length > 0 ->
                // setRaw-only update: no regular kvps needed, raw kvps will be generated below
                [||]
            | None, [] -> failwith "Either an `entity`, `set`, or `setRaw` operations must be present in an `update` expression."
            | None, setValues -> setValues |> List.toArray

        // Process setRaw values into UnsafeLiteral kvps and collect parameter bindings
        let rawKvps = ResizeArray<KeyValuePair<string, obj>>()
        let allRawBindings = ResizeArray<string * obj>()
        let mutable rawParamCounter = 0
        for (col, rawSql, parms) in spec.SetRawValues do
            let mutable processedSql = rawSql
            for p in parms do
                let paramName = $"@__raw_{rawParamCounter}"
                rawParamCounter <- rawParamCounter + 1
                let idx = processedSql.IndexOf('?')
                if idx >= 0 then
                    processedSql <- processedSql.Substring(0, idx) + paramName + processedSql.Substring(idx + 1)
                    allRawBindings.Add(paramName, if isNull p then box System.DBNull.Value else p)
                else
                    failwith $"setRaw: more parameters supplied than '?' placeholders in SQL: '{rawSql}'"
            rawKvps.Add(KeyValuePair(col, UnsafeLiteral(processedSql) :> obj))

        let preparedKvps =
            [| for (k, v) in kvps -> KeyValuePair(k, v)
               yield! rawKvps |]

        let q = Query(spec.Table).AsUpdate(preparedKvps)

        // Store raw parameter bindings for later use in QueryContext
        if allRawBindings.Count > 0 then
            SetRawParamStore.set q (allRawBindings |> Seq.toList)

        // Apply `where` clause
        match spec.Where with
        | Some where -> q.Where(fun w -> where)
        | None -> q

    let fromInsert (spec: InsertQuerySpec<'T, 'InsertReturn>) =
        match spec.InsertType with
        | InsertFromSelect selectQuery ->
            let columns =
                match spec.Fields with
                | [] ->
                    FSharp.Reflection.FSharpType.GetRecordFields(typeof<'T>)
                    |> Array.map (fun p -> p.Name)
                | fields -> fields |> List.toArray
            Query(spec.Table).AsInsert(columns, selectQuery)
        | _ ->

        let includedProperties =
            match spec.Fields with
            | [] ->
                FSharp.Reflection.FSharpType.GetRecordFields(typeof<'T>)
            | fields ->
                let included = fields |> Set.ofList
                FSharp.Reflection.FSharpType.GetRecordFields(typeof<'T>)
                |> Array.filter (fun p -> included.Contains(p.Name))

        match spec.Entities with
        | [] -> 
            failwith "At least one `entity` or `entities` must be set in the `insert` builder."

        | [ entity ] -> 
            let keyValuePairs =
                includedProperties
                |> Array.map (fun p -> KeyValuePair(p.Name, getQueryParameterForEntity entity p))
                |> Array.toList
            Query(spec.Table).AsInsert(keyValuePairs, returnId = spec.IdentityField.IsSome)

        | entities -> 
            if spec.IdentityField.IsSome 
            then failwith "`getId` is not currently supported for multiple inserts via the `entities` operation."
            let columns = includedProperties |> Array.map (fun p -> p.Name)
            let rowsValues =
                entities
                |> List.map (fun entity ->
                    includedProperties
                    |> Array.map (fun p -> getQueryParameterForEntity entity p)
                    |> Array.toSeq
                )
            Query(spec.Table).AsInsert(columns, rowsValues)

    /// Fails if `getId` identity field is used as an `onConflict` target.
    let failIfIdentityOnConflict spec =
        match spec.IdentityField, spec.InsertType with
        | Some ident, OnConflictDoUpdate (conflictFields, _)
        | Some ident, OnConflictDoUpdateCoalesce (conflictFields, _, _)
        | Some ident, OnConflictDoNothing conflictFields
        | Some ident, OnConflictDoNothingWhereRaw (conflictFields, _)
        | Some ident, InsertOrUpdateOnUnique (conflictFields, _) ->
            if conflictFields |> List.contains ident
            then failwith $"Using identity column as a conflict target is not supported."
        | _ -> ()


[<AbstractClass>]
type SelectQuery() = 
    abstract member ToKataQuery : unit -> SqlKata.Query

type SelectQuery<'T>(query: SqlKata.Query) = 
    inherit SelectQuery()
    member this.KataQuery = query
    override this.ToKataQuery() = query

type DeleteQuery<'T>(query: SqlKata.Query) = 
    inherit SelectQuery()
    member this.KataQuery = query
    override this.ToKataQuery() = query

type UpdateQuery<'T, 'UpdateReturn>(spec: UpdateQuerySpec<'T, 'UpdateReturn>) =
    inherit SelectQuery()
    let kataQuery = lazy (spec |> KataUtils.fromUpdate)
    member this.Spec = spec
    member this.KataQuery = kataQuery.Value
    override this.ToKataQuery() = kataQuery.Value

type InsertQuery<'T, 'Identity>(spec: InsertQuerySpec<'T, 'Identity>) =
    inherit SelectQuery()
    let kataQuery = lazy (spec |> KataUtils.fromInsert)
    member this.Spec = spec
    member this.KataQuery = kataQuery.Value
    override this.ToKataQuery() = kataQuery.Value
