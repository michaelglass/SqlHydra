module SqlHydra.Query.Pgvector.PgvectorExtensions

open SqlHydra.Query
open SqlKata

/// Registers pgvector infix operators with the SqlHydra expression visitor.
/// This module auto-initializes when any type in this assembly is accessed.
[<AutoOpen>]
module PgvectorRegistration =
    let private _registered =
        InfixOperators.register "cosine_distance" "<=>"
        InfixOperators.register "l2_distance" "<->"
        InfixOperators.register "inner_product_distance" "<#>"
        true

    /// Call to ensure pgvector infix operators are registered.
    /// Normally not needed as opening the module triggers registration,
    /// but can be called explicitly if needed.
    let ensureRegistered () = _registered |> ignore

/// pgvector distance functions for use in select expressions and order by clauses.
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
