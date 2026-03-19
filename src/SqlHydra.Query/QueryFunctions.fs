namespace SqlHydra.Query

open System

/// Registry for SQL functions that should be emitted as infix operators.
/// Register a function name to emit `left OP right` instead of `fn(left, right)`.
module InfixOperators =
    let private registry = System.Collections.Concurrent.ConcurrentDictionary<string, string>()

    /// Register a function name to be emitted as an infix operator.
    let register (fnName: string) (operator: string) =
        registry.[fnName] <- operator

    /// Look up whether a function should be emitted as an infix operator.
    let tryGetOperator (fnName: string) =
        match registry.TryGetValue(fnName) with
        | true, op -> Some op
        | _ -> None

    // Register built-in infix operators (pgvector distance functions)
    do
        register "cosine_distance" "<=>"
        register "l2_distance" "<->"
        register "inner_product_distance" "<#>"

[<AutoOpen>]
module Table =

    /// Maps the entity 'T to a table of the exact same name.
    let table<'T> =
        let ent = typeof<'T>
        let tables = Map [Root, { Name = ent.Name; Schema = ent.DeclaringType.Name; RecordType = None; CteQuery = None }]
        QuerySource<'T>(tables)

    /// Creates a CTE (Common Table Expression) source from a select query.
    /// When the inner query type matches the CTE type, type inference works automatically.
    let cte<'T> (alias: string) (innerQuery: SelectQuery<'T>) : QuerySource<'T> =
        let tables = Map [Root, { Name = alias; Schema = ""; RecordType = Some typeof<'T>; CteQuery = Some (innerQuery.ToKataQuery()) }]
        QuerySource<'T>(tables)

    /// Creates a CTE source where the outer type differs from the inner query type.
    /// Use when the inner query uses kata SelectRaw for computed columns not in the inner type.
    let cteFrom<'T> (alias: string) (innerQuery: SelectQuery) : QuerySource<'T> =
        let tables = Map [Root, { Name = alias; Schema = ""; RecordType = Some typeof<'T>; CteQuery = Some (innerQuery.ToKataQuery()) }]
        QuerySource<'T>(tables)

    /// Maps the entity 'T to a schema of the given name.
    [<System.Obsolete("The table schema is now automatically inferred from the declaring type.")>]
    let inSchema<'T> (schemaName: string) (qs: QuerySource<'T>) =
        qs

[<AutoOpen>]
module Where = 

    /// WHERE column is IN values
    let isIn<'P> (prop: 'P) (values: 'P seq) = true
    /// WHERE column is IN values
    let inline (|=|) (prop: 'P) (values: 'P seq) = true

    /// WHERE column is NOT IN values
    let isNotIn<'P> (prop: 'P) (values: 'P seq) = true
    /// WHERE column is NOT IN values
    let inline (|<>|) (prop: 'P) (values: 'P seq) = true

    /// WHERE column like value   
    let like<'P> (prop: 'P) (pattern: string) = true
    /// WHERE column like value   
    let inline (=%) (prop: 'P) (pattern: string) = true

    /// WHERE column not like value   
    let notLike<'P> (prop: 'P) (pattern: string) = true
    /// WHERE column not like value   
    let inline (<>%) (prop: 'P) (pattern: string) = true

    /// WHERE column IS NULL
    let isNullValue<'P> (prop: 'P) = true
    /// WHERE column IS NOT NULL
    let isNotNullValue<'P> (prop: 'P) = true

    /// Creates a subquery that returns a single value to be used with column comparisons.
    let subqueryOne (query: SelectQuery<'T>) : 'T = Unchecked.defaultof<'T>

    /// Creates a subquery that returns many values to be used with "isIn", "isNotIn", "|=|" or "|<>|".
    let subqueryMany (query: SelectQuery<'T>) : 'T list = []

    /// Compares two values for equality.
    let areEqual (prop: 'P) (value: 'P) = true

    /// Compares two values for inequality.
    let notEqual (prop: 'P) (value: 'P) = true

[<AutoOpen>]
module OrderBy = 

    // infix operator ^^ that takes a boolean that conditionally includes the sort property.
    let inline (^^) (_: bool) (prop: 'P) =
        prop

(*
Select Aggregates:

countBy, avgBy, minBy, maxBy, sumBy

select {
    for p in productsTable do
    join c in categoryTable on (p.ProductCategoryID.Value = c.ProductCategoryID)
    groupBy p.Department
    select p.Department, minBy p.Price, maxBy p.Price
}

SELECT [SalesLT].[Product].[Department], MIN([SalesLT].[Product].[Price]) AS MinPrice, MAX([SalesLT].[Product].[Price]) AS MaxPrice
*)

[<AutoOpen>]
module Aggregates =

    /// Gets the COUNT of the given column
    let countBy (prop: 'P) = Unchecked.defaultof<int>

    /// Gets the MIN of the given column
    let minBy (prop: 'P) = Unchecked.defaultof<'P>

    /// Gets the MAX of the given column
    let maxBy (prop: 'P) = Unchecked.defaultof<'P>

    /// Gets the SUM of the given column
    let sumBy (prop: 'P when 'P : struct) = Unchecked.defaultof<'P>

    /// Gets the AVG of the given column
    let avgBy (prop: 'P when 'P : struct) = Unchecked.defaultof<'P>

    /// Gets the AVG of the given column and returns 'Result.
    let avgByAs<'P, 'Result when 'P : struct and 'Result : struct> (prop: 'P) : 'Result = Unchecked.defaultof<'Result>

    /// Gets the COUNT of DISTINCT values of the given column
    let countDistinct (prop: 'P) = Unchecked.defaultof<int>

[<AutoOpen>]
module SqlFunctions =

    /// A stub value used to define SQL function wrappers.
    /// The function name and arguments are translated directly to SQL.
    /// Example:
    ///   let LEN (s: string) : int = sqlFn
    ///   let SUBSTRING (s: string, start: int, length: int) : string = sqlFn
    let sqlFn<'Return> : 'Return = Unchecked.defaultof<'Return>

/// Standard SQL functions for use in select expressions.
/// Use `open type SqlFn` to access functions without qualification.
type SqlFn =
    // Null handling
    static member coalesce(a: Option<'T>, b: 'T) : 'T = sqlFn
    static member coalesce(a: Nullable<'T>, b: 'T) : 'T when 'T : struct = sqlFn
    static member coalesce(a: 'T, b: 'T) : 'T = sqlFn
    static member coalesce(a: 'T, b: 'T, c: 'T) : 'T = sqlFn
    static member nullif(a: 'T, b: 'T) : Option<'T> = sqlFn

    // Numeric functions (standard SQL)
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

    // String functions (standard SQL names)
    static member upper(s: string) : string = sqlFn
    static member lower(s: string) : string = sqlFn
    static member trim(s: string) : string = sqlFn
    static member substring(s: string, start: int, length: int) : string = sqlFn
    static member replace(s: string, from: string, ``to``: string) : string = sqlFn
    static member concat(s1: string, s2: string) : string = sqlFn
    static member concat(s1: string, s2: string, s3: string) : string = sqlFn

    // GREATEST / LEAST (standard SQL)
    static member greatest(a: 'T, b: 'T) : 'T = sqlFn
    static member greatest(a: 'T, b: 'T, c: 'T) : 'T = sqlFn
    static member greatest(a: 'T, b: 'T, c: 'T, d: 'T) : 'T = sqlFn
    static member least(a: 'T, b: 'T) : 'T = sqlFn
    static member least(a: 'T, b: 'T, c: 'T) : 'T = sqlFn
    static member least(a: 'T, b: 'T, c: 'T, d: 'T) : 'T = sqlFn

[<AutoOpen>]
module CastFunctions =
    /// CAST(expression AS targetType).
    /// The target SQL type is inferred from the F# return type:
    /// float/double → FLOAT, int → INTEGER, int64 → BIGINT, decimal → NUMERIC, string → TEXT, bool → BOOLEAN.
    let castAs<'Result> (value: 'T) : 'Result = Unchecked.defaultof<'Result>

[<AutoOpen>]
module CaseWhenFunctions =
    /// CASE WHEN condition THEN thenValue ELSE elseValue END.
    /// Note: values are rendered as SQL literals, not parameters.
    /// Column references are properly qualified. Do not pass unsanitized user input.
    let caseWhen<'T> (condition: bool) (thenValue: 'T) (elseValue: 'T) : 'T = Unchecked.defaultof<'T>

    /// Multi-branch CASE WHEN expression.
    /// CASE WHEN cond1 THEN val1 WHEN cond2 THEN val2 ... ELSE elseVal END.
    let caseWhenMulti<'T> (branches: (bool * 'T) list) (elseValue: 'T) : 'T = Unchecked.defaultof<'T>

[<AutoOpen>]
module ParamFunctions =
    /// Injects an external F# value as a SQL parameter in a SELECT projection.
    /// Use in INSERT ... SELECT to mix table columns with external values.
    let inlineValue<'T> (value: 'T) : 'T = Unchecked.defaultof<'T>
