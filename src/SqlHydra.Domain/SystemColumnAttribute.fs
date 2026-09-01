namespace SqlHydra

open System

/// Marks a generated field as a read-only [system column](https://www.postgresql.org/docs/current/ddl-system-columns.html).
/// Appended by name to a whole-entity `select` (`SELECT *` excludes it); never written.
[<AttributeUsage(AttributeTargets.Property
                 ||| AttributeTargets.Field)>]
type SystemColumnAttribute() =
    inherit Attribute()
