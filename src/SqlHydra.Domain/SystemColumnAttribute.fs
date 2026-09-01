namespace SqlHydra

open System

/// Marks a generated record field as a read-only, database-managed *system column*
/// (PostgreSQL's `xmin`, `ctid`, and the rest). SqlHydra.Query treats such a column as:
///   * appended by name to a whole-entity `select`, because `SELECT *` excludes it and a
///     record that declares the field would otherwise fail to hydrate on every read, and
///   * absent from every INSERT column list and UPDATE SET clause, because the database
///     owns the value,
/// while leaving it an ordinary column everywhere else — `where (u.xmin = expected)` is
/// just a comparison.
[<AttributeUsage(AttributeTargets.Property
                 ||| AttributeTargets.Field)>]
type SystemColumnAttribute() =
    inherit Attribute()
