namespace SqlHydra.Query

[<AutoOpen>]
module Table = 

    /// Maps the entity 'T to a table of the exact same name.
    let table<'T> =
        let ent = typeof<'T>
        let tables = Map [Root, { Name = ent.Name; Schema = ent.DeclaringType.Name; RecordType = None; CteQuery = None }]
        QuerySource<'T>(tables)

    /// Creates a CTE (Common Table Expression) source from a select query.
    /// Use with anonymous records for named column access without boilerplate types.
    let cte<'T> (alias: string) (innerQuery: SelectQuery<'T>) : QuerySource<'T> =
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

[<AutoOpen>]
module SqlFunctions =

    /// A stub value used to define SQL function wrappers.
    /// The function name and arguments are translated directly to SQL.
    /// Example:
    ///   let LEN (s: string) : int = sqlFn
    ///   let SUBSTRING (s: string, start: int, length: int) : string = sqlFn
    let sqlFn<'Return> : 'Return = Unchecked.defaultof<'Return>

[<AutoOpen>]
module CaseWhenFunctions =
    /// CASE WHEN condition THEN thenValue ELSE elseValue END.
    /// Note: values are rendered as SQL literals, not parameters.
    /// Column references are properly qualified. Do not pass unsanitized user input.
    let caseWhen<'T> (condition: bool) (thenValue: 'T) (elseValue: 'T) : 'T = Unchecked.defaultof<'T>

[<AutoOpen>]
module ParamFunctions =
    /// Injects an external F# value as a SQL parameter in a SELECT projection.
    /// Use in INSERT ... SELECT to mix table columns with external values.
    let inlineValue<'T> (value: 'T) : 'T = Unchecked.defaultof<'T>
