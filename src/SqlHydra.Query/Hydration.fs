/// Replaces the old SqlHydra.Cli generated `HydraReader` class with an internal module.
module SqlHydra.Query.Hydration

open System
open System.Data.Common
open FSharp.Reflection
open SqlHydra.Domain

/// Tracks ordinal position across joined tables.
[<Sealed>]
type OrdinalTracker(reader: DbDataReader) =
    let mutable accFieldCount = 0

    member _.Reader = reader

    /// Builds an ordinal lookup for a record type's fields, advancing the ordinal counter.
    member _.BuildGetOrdinal(tableType: Type) =
        let fieldNames =
            FSharpType.GetRecordFields(tableType)
            |> Array.map _.Name

        let dictionary =
            [| 0 .. reader.FieldCount - 1 |]
            |> Array.map (fun i -> reader.GetName(i), i)
            |> Array.sortBy snd
            |> Array.skip accFieldCount
            |> Array.filter (fun (name, _) -> Array.contains name fieldNames)
            |> Array.take fieldNames.Length
            |> dict
        accFieldCount <- accFieldCount + fieldNames.Length
        dictionary

    /// Gets the next ordinal and increments the counter.
    member _.GetOrdinalAndIncrement() =
        let ordinal = accFieldCount
        accFieldCount <- accFieldCount + 1
        ordinal

/// Module containing methods accessed via reflection. Must be a static class.
/// Contains some provider-specific exception handling for reading column values.
type ColumnReadMethods private () =
    static let mutable provider = SqlServer
    static member SetProvider(p: ProviderType) = provider <- p

    static member private ConvertOracleProviderType<'T>(value: obj) : 'T =
        let actual = value.GetType()
        let t = typeof<'T>

        // -------------------------
        // OracleDecimal
        // -------------------------
        if actual.Name = "OracleDecimal" then
            let isNull = actual.GetProperty("IsNull").GetValue(value) :?> bool
            if isNull then Unchecked.defaultof<'T>
            else
                if t = typeof<decimal> then
                    actual.GetProperty("Value").GetValue(value) :?> 'T
                elif t = typeof<int> then
                    actual.GetMethod("ToInt32").Invoke(value, [||]) :?> 'T
                elif t = typeof<int64> then
                    actual.GetMethod("ToInt64").Invoke(value, [||]) :?> 'T
                elif t = typeof<double> then
                    actual.GetMethod("ToDouble").Invoke(value, [||]) :?> 'T
                elif t = typeof<bool> then
                    let i = actual.GetMethod("ToInt32").Invoke(value, [||]) :?> int
                    (i = 1) :> obj :?> 'T
                else
                    failwithf "Unsupported OracleDecimal → %s" t.FullName

        // -------------------------
        // OracleDate
        // -------------------------
        elif actual.Name = "OracleDate" then
            let isNull = actual.GetProperty("IsNull").GetValue(value) :?> bool
            if isNull then Unchecked.defaultof<'T>
            else
                actual.GetProperty("Value").GetValue(value) :?> 'T

        // -------------------------
        // OracleTimeStamp / OracleTimeStampTZ
        // -------------------------
        elif actual.Name.StartsWith("OracleTimeStamp") then
            let isNull = actual.GetProperty("IsNull").GetValue(value) :?> bool
            if isNull then Unchecked.defaultof<'T>
            else
                actual.GetProperty("Value").GetValue(value) :?> 'T

        // -------------------------
        // Fallback
        // -------------------------
        else
            value :?> 'T

    static member private ConvertClrOracleValue<'T>(value: obj) : 'T =
        let t = typeof<'T>

        match value with
        // double → numeric
        | :? double as d when t = typeof<decimal> ->
            decimal d :> obj :?> 'T
        | :? double as d when t = typeof<int> ->
            int d :> obj :?> 'T
        | :? double as d when t = typeof<int64> ->
            int64 d :> obj :?> 'T

        // int64 → numeric
        | :? int64 as i when t = typeof<int> ->
            int i :> obj :?> 'T

        // decimal → numeric
        | :? decimal as d when t = typeof<int> ->
            int d :> obj :?> 'T
        | :? decimal as d when t = typeof<int64> ->
            int64 d :> obj :?> 'T
        | :? decimal as d when t = typeof<double> ->
            double d :> obj :?> 'T

        // fallback
        | _ ->
            value :?> 'T


    static member private ConvertOracleValue<'T>(value: obj) : 'T =
        if value = null || value = DBNull.Value then
            Unchecked.defaultof<'T>
        else
            let actual = value.GetType()
            let t = typeof<'T>

            // Case 1: Oracle provider types
            if actual.Namespace = "Oracle.ManagedDataAccess.Types" then
                ColumnReadMethods.ConvertOracleProviderType<'T>(value)

            // Case 2: Oracle returned a CLR primitive (double, int64, etc.)
            else 
                ColumnReadMethods.ConvertClrOracleValue<'T>(value)


    /// Reads a value from the data reader using `GetFieldValue<T>`, with a provider‑specific fallback for Oracle. Oracle’s ADO.NET provider does not return normal CLR types for many column kinds (such as NUMBER, DATE, and TIMESTAMP). Instead, it exposes provider‑specific wrapper types like OracleDecimal, OracleDate, and OracleTimeStampTZ. These cannot be cast to CLR primitives, so `GetFieldValue<T>` throws an `InvalidCastException`.  
    /// To keep the hydration pipeline provider‑agnostic, this method catches that specific exception when the active provider is Oracle and converts the underlying Oracle value to the requested CLR type using reflection. All other providers use the standard `GetFieldValue<T>` path.  
    /// This isolates Oracle‑specific behavior in one place while preserving a clean, generic hydration model for all other providers.
    static member private TryGetFieldValue<'T>(reader: DbDataReader, ordinal: int) : 'T =
        try
            reader.GetFieldValue<'T>(ordinal)
        with
        | :? InvalidCastException when provider = Oracle ->
            ColumnReadMethods.ConvertOracleValue<'T>(reader.GetValue(ordinal))
#if NET6_0_OR_GREATER
        | (:? InvalidCastException | :? FormatException) when typeof<'T> = typeof<DateOnly> ->
            // SQLite (and possibly other providers) may store dates as datetime strings
            // (e.g. '2022-01-24 00:00:00') which DateOnly.Parse/GetFieldValue rejects.
            // Fall back to reading as DateTime and converting.
            let dt = reader.GetDateTime(ordinal)
            DateOnly.FromDateTime(dt) :> obj :?> 'T
        | (:? InvalidCastException | :? FormatException) when typeof<'T> = typeof<TimeOnly> ->
            let dt = reader.GetDateTime(ordinal)
            TimeOnly.FromDateTime(dt) :> obj :?> 'T
#endif

    static member ReadRequired<'T>(reader: DbDataReader, ordinal: int) : obj =
        ColumnReadMethods.TryGetFieldValue<'T>(reader, ordinal) |> box

    static member ReadOption<'T>(reader: DbDataReader, ordinal: int) : obj =
        if reader.IsDBNull(ordinal) then box None
        else ColumnReadMethods.TryGetFieldValue<'T>(reader, ordinal) |> Some |> box

    static member ReadNullableStruct<'T when 'T : struct and 'T :> ValueType and 'T : (new: unit -> 'T)>(reader: DbDataReader, ordinal: int) : obj =
        if reader.IsDBNull(ordinal) then Nullable() |> box
        else Nullable(ColumnReadMethods.TryGetFieldValue<'T>(reader, ordinal)) |> box

    static member ReadNullableObj<'T when 'T : not struct>(reader: DbDataReader, ordinal: int) : obj =
        if reader.IsDBNull(ordinal) then null |> box
        else ColumnReadMethods.TryGetFieldValue<'T>(reader, ordinal) |> box

/// Determines if a type is Option<_>.
let private isOptionType (t: Type) =
    t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Option<_>>

/// SPIKE (design B): is this SqlHydra.ReadOnly<_>?
let private isReadOnlyWrapper (t: Type) =
    t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<SqlHydra.ReadOnly<_>>

/// SPIKE (design B): builds a ReadOnly<'T> around an already-read value.
let private readOnlyWrapper (wrapperType: Type) =
    let ctor = FSharpValue.PreComputeRecordConstructor(wrapperType)
    fun (v: obj) -> ctor [| v |]

/// Determines if a type is Nullable<_>.
let private isNullableType (t: Type) =
    t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Nullable<_>>

/// Unwraps Option<T> or Nullable<T> to get the inner type T.
let private unwrapType (t: Type) =
    if isOptionType t then t.GenericTypeArguments.[0]
    elif isNullableType t then t.GenericTypeArguments.[0]
    elif isReadOnlyWrapper t then t.GenericTypeArguments.[0]   // SPIKE (design B)
    else t

/// Checks if a type is a primitive/scalar (not an F# record).
let private isPrimitive (t: Type) =
    let inner = unwrapType t
    not (FSharpType.IsRecord inner)

/// Gets a function that reads a single column value from the reader at a given ordinal.
let private makeColumnReader (reader: DbDataReader) (baseType: Type) (isOpt: bool) (isNullable: bool) : (int -> obj) =
    let methodName =
        if isOpt then "ReadOption"
        elif isNullable && baseType.IsValueType then "ReadNullableStruct"
        elif isNullable && not baseType.IsValueType then "ReadNullableObj"
        else "ReadRequired"

    let methodInfo =
        typeof<ColumnReadMethods>.GetMethod(methodName).MakeGenericMethod(baseType)

    fun (ordinal: int) ->
        methodInfo.Invoke(null, [| reader :> obj; ordinal :> obj |])

/// Builds field readers for a record type using pre-computed ordinals.
let private buildRecordFieldReaders (reader: DbDataReader) (recordType: Type) (ordinalLookup: System.Collections.Generic.IDictionary<string, int>) =
    let fields = FSharpType.GetRecordFields(recordType)
    fields
    |> Array.map (fun pi ->
        let fieldType = pi.PropertyType
        let isOpt = isOptionType fieldType
        let isNullable = isNullableType fieldType
        let baseType = unwrapType fieldType
        // Reference types (e.g. string, byte[]) can be NULL in SQL even without Option/Nullable wrapper
        let isNullable = isNullable || (not isOpt && baseType.IsClass)
        // SPIKE (design B): a ReadOnly<'T> field reads as 'T, then gets wrapped.
        // NOTE: `Option<ReadOnly<'T>>` (a NULLable read-only column) needs its own
        // branch — unwrapType strips one level, so the generic path below would hand
        // ReadOnly<'T> to GetFieldValue and only blow up on rows where the column is
        // NOT null. That is how this was found.
        let columnReader =
            if isOpt && isReadOnlyWrapper (unwrapType fieldType) then
                let w = unwrapType fieldType
                let inner = unwrapType w
                let raw = makeColumnReader reader inner false false
                let wrap = readOnlyWrapper w
                let cases = FSharpType.GetUnionCases(fieldType)
                let someCase = cases |> Array.find (fun c -> c.Name = "Some")
                let noneValue =
                    FSharpValue.MakeUnion(cases |> Array.find (fun c -> c.Name = "None"), [||])
                fun (ordinal: int) ->
                    if reader.IsDBNull(ordinal) then noneValue
                    else FSharpValue.MakeUnion(someCase, [| wrap (raw ordinal) |])
            elif isReadOnlyWrapper fieldType then
                let wrap = readOnlyWrapper fieldType
                (makeColumnReader reader baseType isOpt isNullable) >> wrap
            else
                makeColumnReader reader baseType isOpt isNullable
        let ordinal = ordinalLookup.[pi.Name]
        (ordinal, columnReader)
    )

/// Builds a read function for a single entity type that may be:
/// - A primitive/scalar type (Option<int>, string, int, etc.)
/// - An Option<Record> (for left joins)
/// - A record type
let private buildEntityReadFn (tracker: OrdinalTracker) (entityType: Type) : (unit -> obj) =
    let reader = tracker.Reader
    let isOpt = isOptionType entityType
    let isNullable = isNullableType entityType
    let innerType = unwrapType entityType

    // SPIKE (design B): a bare ReadOnly<'T> select projection is a scalar, not a joined table.
    let roWrapper =
        if isReadOnlyWrapper entityType then Some entityType
        elif (isOptionType entityType || isNullableType entityType)
             && isReadOnlyWrapper entityType.GenericTypeArguments.[0] then
            Some entityType.GenericTypeArguments.[0]
        else None

    match roWrapper with
    | Some w ->
        let baseType = unwrapType w
        let columnReader = makeColumnReader reader baseType isOpt isNullable
        let wrap = readOnlyWrapper w
        let ordinal = tracker.GetOrdinalAndIncrement()
        fun () -> wrap (columnReader ordinal)
    | None ->

    if FSharpType.IsRecord innerType then
        // Record type (possibly wrapped in Option for left joins)
        let ordinalLookup = tracker.BuildGetOrdinal(innerType)
        let fieldReaders = buildRecordFieldReaders reader innerType ordinalLookup

        if isOpt then
            // Option<Record> — left join: check first column for DBNull → return None
            let firstOrdinal = fst fieldReaders.[0]
            let someCase = FSharpType.GetUnionCases(entityType) |> Array.find (fun c -> c.Name = "Some")
            let noneCase = FSharpType.GetUnionCases(entityType) |> Array.find (fun c -> c.Name = "None")
            let noneValue = FSharpValue.MakeUnion(noneCase, [||])

            fun () ->
                if reader.IsDBNull(firstOrdinal) then
                    noneValue
                else
                    let values = fieldReaders |> Array.map (fun (ord, read) -> read ord)
                    let record = FSharpValue.MakeRecord(innerType, values)
                    FSharpValue.MakeUnion(someCase, [| record |])
        else
            // Plain record type
            fun () ->
                let values = fieldReaders |> Array.map (fun (ord, read) -> read ord)
                FSharpValue.MakeRecord(innerType, values)
    else
        // Scalar/primitive type (possibly wrapped in Option/Nullable)
        let baseType = unwrapType entityType
        let columnReader = makeColumnReader reader baseType isOpt isNullable
        let ordinal = tracker.GetOrdinalAndIncrement()
        fun () -> columnReader ordinal

/// Builds a function that reads one row from the reader and returns 'T.
/// Called once per query (after reader is opened), returned fn is called per row.
let buildRowReader<'T> (provider: ProviderType) (reader: DbDataReader) : (unit -> 'T) =
    let t = typeof<'T>
    let tracker = OrdinalTracker(reader)
    ColumnReadMethods.SetProvider(provider)

    if FSharpType.IsTuple(t) then
        let elementTypes = FSharpType.GetTupleElements(t)
        let readFns = elementTypes |> Array.map (buildEntityReadFn tracker)
        fun () ->
            let values = readFns |> Array.map (fun read -> read())
            FSharpValue.MakeTuple(values, t) :?> 'T
    else
        let readFn = buildEntityReadFn tracker t
        fun () ->
            readFn() :?> 'T

