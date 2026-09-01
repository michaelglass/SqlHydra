namespace SqlHydra

open System

/// Marks a generated field as one the database writes: SqlHydra.Query leaves it out of
/// every INSERT column list and UPDATE SET clause. PostgreSQL rejects a write naming a
/// [generated column](https://www.postgresql.org/docs/current/ddl-generated-columns.html).
[<AttributeUsage(AttributeTargets.Property
                 ||| AttributeTargets.Field)>]
type ReadOnlyColumnAttribute() =
    inherit Attribute()

    /// Why the database refuses a statement naming the column, and what the caller can write
    /// instead. Each kind refuses for its own reason and offers its own way out, so the
    /// sentence travels with the kind rather than being written once at the raise.
    abstract Refusal: string
    default _.Refusal =
        "the database generates its value and rejects a statement that names it. Remove the `set`, or `setRaw` it to DEFAULT, the one value the database accepts"
