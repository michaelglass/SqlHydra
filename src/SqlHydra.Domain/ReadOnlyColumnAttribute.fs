namespace SqlHydra

open System

/// Marks a generated field as one the database writes: SqlHydra.Query leaves it out of
/// every INSERT column list and UPDATE SET clause. PostgreSQL rejects a write naming a
/// [generated column](https://www.postgresql.org/docs/current/ddl-generated-columns.html).
[<AttributeUsage(AttributeTargets.Property
                 ||| AttributeTargets.Field)>]
type ReadOnlyColumnAttribute() =
    inherit Attribute()
