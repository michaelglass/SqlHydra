namespace SqlHydra.Query

open System

/// Comparison operators used in WHERE/HAVING/ON clauses.
type ComparisonOp =
    | Eq
    | NotEq
    | Gt
    | GtEq
    | Lt
    | LtEq

/// Logical connective for combining predicates.
type LogicalOp =
    | And
    | Or

/// JOIN type.
type JoinKind =
    | InnerJoin
    | LeftJoin
    /// LEFT JOIN LATERAL (...) — for subquery-shaped joins (PostgreSQL).
    | LeftJoinLateral

/// ORDER BY direction.
type OrderDirection =
    | Asc
    | Desc

/// NULLS ordering for ORDER BY (PostgreSQL/Oracle semantics).
type NullsOrdering =
    | NullsDefault
    | NullsFirst
    | NullsLast

/// An ORDER BY clause.
type OrderByClause =
    | OrderByColumn of column: string * direction: OrderDirection
    /// ORDER BY column with explicit NULLS FIRST/LAST.
    | OrderByColumnNulls of column: string * direction: OrderDirection * nulls: NullsOrdering
    | OrderByRaw of fragment: string

/// A SELECT column.
type SelectColumn =
    /// Select all columns from a table alias: alias.*
    | AllColumns of tableAlias: string
    /// Select a specific column: alias.column
    | SpecificColumn of qualifiedName: string
    /// Raw SQL expression (e.g., COUNT(*), aggregate AS alias)
    | RawColumn of fragment: string
    /// Raw SQL expression with bound parameters. `?` placeholders are replaced by parameter names in order.
    | RawColumnWithParams of fragment: string * parameters: obj[]

// ─── Mutually recursive types ───
// SqlValue, WhereClause, JoinClause, and SelectQueryIR reference each other.

/// A SQL expression value (right-hand side of a comparison).
type SqlValue =
    /// A parameter value (may be a QueryParameter wrapping provider type info)
    | Parameter of value: obj
    /// SQL NULL
    | Null
    /// A reference to another column (fully qualified: alias.column or schema.table.column)
    | ColumnRef of qualifiedName: string
    /// A subquery reference
    | SubQuery of SelectQueryIR
    /// Raw SQL fragment with parameter bindings
    | RawSql of fragment: string * parameters: obj[]

/// A predicate in a WHERE, HAVING, or JOIN ON clause.
and WhereClause =
    /// column op value (e.g., a.City = @p0)
    | Compare of column: string * op: ComparisonOp * value: SqlValue
    /// column op column (e.g., a.Id = b.Id)
    | CompareColumns of left: string * op: ComparisonOp * right: string
    /// column IS NULL
    | IsNull of column: string
    /// column IS NOT NULL
    | IsNotNull of column: string
    /// column IN (value1, value2, ...)
    | InValues of column: string * values: obj[]
    /// column IN (subquery)
    | InSubQuery of column: string * subquery: SelectQueryIR
    /// column NOT IN (value1, value2, ...)
    | NotInValues of column: string * values: obj[]
    /// column NOT IN (subquery)
    | NotInSubQuery of column: string * subquery: SelectQueryIR
    /// column LIKE pattern
    | Like of column: string * pattern: obj
    /// column NOT LIKE pattern
    | NotLike of column: string * pattern: obj
    /// EXISTS (subquery)
    | Exists of subquery: SelectQueryIR
    /// NOT EXISTS (subquery)
    | NotExists of subquery: SelectQueryIR
    /// NOT (clause)
    | Not of WhereClause
    /// left AND/OR right
    | Combined of left: WhereClause * op: LogicalOp * right: WhereClause
    /// Raw WHERE SQL fragment with parameter bindings
    | RawWhere of fragment: string * parameters: obj[]
    /// Boolean column check (e.g., WHERE a.IsActive = true)
    | BoolColumn of column: string * value: bool
    /// Wraps a clause in parentheses (used by WHERE builder to group conditions)
    | Grouped of WhereClause
    /// Identity element - no condition
    | Empty

/// A JOIN clause.
and JoinClause = {
    Kind: JoinKind
    /// Table spec string, e.g. "Sales.SalesOrderDetail AS d", or the alias when Subquery is Some.
    Table: string
    /// When Some, render `<JoinKind> [LATERAL] (subquery) AS <Table>` instead of using Table as the spec.
    Subquery: SelectQueryIR option
    /// Join conditions
    Condition: WhereClause
}

/// The complete SELECT query IR.
and SelectQueryIR = {
    /// CTEs to render before SELECT: WITH alias AS (...).
    WithCtes: (string * SelectQueryIR) list
    /// Table spec: "Schema.Table as alias" or "Schema.Table"
    From: string option
    /// Columns to select. Empty list = SELECT *
    Select: SelectColumn list
    /// WHERE clause. Empty = no WHERE.
    Where: WhereClause
    /// JOIN clauses
    Joins: JoinClause list
    /// GROUP BY column names
    GroupBy: string list
    /// HAVING clause. Empty = no HAVING.
    Having: WhereClause
    /// ORDER BY clauses
    OrderBy: OrderByClause list
    /// OFFSET (skip) value
    Skip: int option
    /// LIMIT (take) value
    Take: int option
    /// DISTINCT flag
    Distinct: bool
    /// DISTINCT ON columns (PostgreSQL). Empty = not used. Mutually exclusive with plain Distinct in practice.
    DistinctOn: string list
    /// SELECT COUNT(*) flag
    IsCount: bool
    /// Carrier for DELETE ... RETURNING column list when this IR is being used by the DELETE builder.
    /// Ignored when emitting SELECT.
    Returning: string list
}

/// Helpers for composing WhereClause values.
module WhereClause =
    /// Combines two WHERE clauses with AND, wrapping each side in Grouped for proper parenthesization.
    let combineAnd (existing: WhereClause) (newClause: WhereClause) =
        match existing, newClause with
        | Empty, c | c, Empty -> c
        | l, r -> Combined(Grouped l, And, Grouped r)

    /// Combines two WHERE clauses with OR, wrapping each side in Grouped for proper parenthesization.
    let combineOr (existing: WhereClause) (newClause: WhereClause) =
        match existing, newClause with
        | Empty, c | c, Empty -> c
        | l, r -> Combined(Grouped l, Or, Grouped r)

    /// Combines two clauses with AND without grouping (flat, for JOIN ON conditions).
    let combineAndFlat (existing: WhereClause) (newClause: WhereClause) =
        match existing, newClause with
        | Empty, c | c, Empty -> c
        | l, r -> Combined(l, And, r)

module SelectQueryIR =
    let empty = {
        WithCtes = []
        From = None
        Select = []
        Where = Empty
        Joins = []
        GroupBy = []
        Having = Empty
        OrderBy = []
        Skip = None
        Take = None
        Distinct = false
        DistinctOn = []
        IsCount = false
        Returning = []
    }

// ─── Insert-related types ───

/// SET clause for UPDATE: either a typed column-value pair, or a raw SQL fragment with parameters.
type SetClause =
    /// SET col = @p
    | SetColumn of column: string * value: obj
    /// SET col = <fragment>; fragment may use `?` placeholders that bind to parameters in order.
    | SetRaw of column: string * fragment: string * parameters: obj[]

type InsertType =
    | Insert
    | InsertOrReplace
    | OnConflictDoUpdate of conflictFields: string list * updateFields: string list
    /// ON CONFLICT (cols) DO UPDATE SET col = COALESCE(EXCLUDED.col, table.col) — keeps existing non-null values.
    | OnConflictDoUpdateCoalesce of conflictFields: string list * updateFields: string list
    | OnConflictDoNothing of conflictFields: string list
    /// ON CONFLICT (cols) WHERE <whereExpr> DO NOTHING — partial-index conflict handling.
    | OnConflictDoNothingWhereRaw of conflictFields: string list * whereFragment: string * parameters: obj[]
    /// ON CONFLICT (<rawTargetExpr>) DO NOTHING — for expression indexes (e.g. lower(email)).
    | OnConflictDoNothingRawTarget of rawTargetExpr: string
    | InsertOrUpdateOnUnique of keyFields: string list * updateFields: string list

type Nullability =
    | IsOptional
    | IsNullable
    | NotNullable

type OutputField =
    {
        ColumnName: string
        PropertyType: Type
        Nullability: Nullability
    }

/// INSERT query IR.
type InsertQueryIR = {
    Table: string
    Columns: string list
    /// Each row is an array of parameter values (may include QueryParameter wrappers)
    Rows: obj[] list
    /// When Some, INSERT INTO ... (cols) <select-subquery> instead of VALUES (...).
    FromSelect: SelectQueryIR option
    IdentityField: string option
    InsertType: InsertType
    OutputFields: OutputField list
    /// PostgreSQL/SQLite RETURNING column list. Empty = no RETURNING.
    Returning: string list
}

/// UPDATE query IR.
type UpdateQueryIR = {
    Table: string
    /// Column name * parameter value pairs
    SetColumns: (string * obj) list
    /// Raw SET clauses: (column, fragment, parameters). Rendered after SetColumns.
    SetRaws: (string * string * obj[]) list
    Where: WhereClause
    OutputFields: OutputField list
    /// PostgreSQL RETURNING column list. Empty = no RETURNING.
    Returning: string list
}

/// DELETE query IR.
type DeleteQueryIR = {
    Table: string
    Where: WhereClause
    /// PostgreSQL RETURNING column list. Empty = no RETURNING.
    Returning: string list
}
