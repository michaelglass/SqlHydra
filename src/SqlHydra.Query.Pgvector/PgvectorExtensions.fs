module SqlHydra.Query.Pgvector.PgvectorExtensions

open System
open System.Linq.Expressions
open SqlHydra.Query

/// Registers pgvector infix operators with the SqlHydra expression visitor.
[<AutoOpen>]
module PgvectorRegistration =
    let private _doRegister =
        InfixOperators.register "cosine_distance" "<=>"
        InfixOperators.register "l2_distance" "<->"
        InfixOperators.register "inner_product_distance" "<#>"

    /// Call to ensure pgvector infix operators are registered.
    /// This is a no-op but forces module initialization.
    let ensureRegistered () = _doRegister

/// pgvector distance functions for use in select expressions.
/// Use `open type PgvectorFn` to access functions without qualification.
/// These emit PostgreSQL pgvector infix operators: <=> (cosine), <-> (L2), <#> (inner product).
type PgvectorFn =
    /// Cosine distance between two vectors. Emits: lhs <=> rhs
    static member cosine_distance(a: 'T, b: 'U) : float = sqlFn
    /// L2 (Euclidean) distance between two vectors. Emits: lhs <-> rhs
    static member l2_distance(a: 'T, b: 'U) : float = sqlFn
    /// Inner product distance between two vectors. Emits: lhs <#> rhs
    static member inner_product_distance(a: 'T, b: 'U) : float = sqlFn

/// pgvector-specific extensions for the select builder.
type SelectBuilder<'Selected, 'Mapped> with

    member private this.OrderByVectorDistance (state: QuerySource<'T, SelectQueryIR>, propertySelector, operator: string, vector: obj) =
        let result = LinqExpressionVisitors.visitOrderByPropertySelector<'T, 'Prop> propertySelector
        match result with
        | LinqExpressionVisitors.OrderByColumn (tableAlias, p) ->
            let fqCol = $"\"{tableAlias}\".\"{p.Name}\""
            QuerySource<'T, SelectQueryIR>(
                { state.Query with
                    OrderBy = state.Query.OrderBy @ [OrderByRawWithParams ($"{fqCol} {operator} ?", [| vector |])] },
                state.TableMappings)
        | LinqExpressionVisitors.OrderByIgnored -> state
        | _ -> failwith "pgvector distance ordering requires a column reference, not an aggregate"

    /// ORDER BY column <=> @vector (pgvector cosine distance, ascending — closest first).
    [<CustomOperation("orderByCosineDistance", MaintainsVariableSpace = true)>]
    member this.OrderByCosineDistance (state: QuerySource<'T, SelectQueryIR>, [<ProjectionParameter>] propertySelector: Expression<Func<'T, 'Prop>>, vector: obj) =
        this.OrderByVectorDistance(state, propertySelector, "<=>", vector)

    /// ORDER BY column <-> @vector (pgvector L2/Euclidean distance, ascending — closest first).
    [<CustomOperation("orderByL2Distance", MaintainsVariableSpace = true)>]
    member this.OrderByL2Distance (state: QuerySource<'T, SelectQueryIR>, [<ProjectionParameter>] propertySelector: Expression<Func<'T, 'Prop>>, vector: obj) =
        this.OrderByVectorDistance(state, propertySelector, "<->", vector)

    /// ORDER BY column <#> @vector (pgvector inner product distance, ascending).
    [<CustomOperation("orderByInnerProductDistance", MaintainsVariableSpace = true)>]
    member this.OrderByInnerProductDistance (state: QuerySource<'T, SelectQueryIR>, [<ProjectionParameter>] propertySelector: Expression<Func<'T, 'Prop>>, vector: obj) =
        this.OrderByVectorDistance(state, propertySelector, "<#>", vector)
