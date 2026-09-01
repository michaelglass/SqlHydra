namespace SqlHydra

open System

/// Marks a field the database writes: SqlHydra.Query leaves it out of every INSERT column
/// list and UPDATE SET clause. https://www.postgresql.org/docs/current/ddl-generated-columns.html
[<AttributeUsage(AttributeTargets.Property
                 ||| AttributeTargets.Field)>]
type ReadOnlyColumnAttribute() =
    inherit Attribute()
