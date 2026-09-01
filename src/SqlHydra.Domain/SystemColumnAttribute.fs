namespace SqlHydra

open System

/// Marks a generated field as a PostgreSQL
/// [system column](https://www.postgresql.org/docs/current/ddl-system-columns.html):
/// `SELECT *` does not return it, so a whole-entity `select` names it explicitly.
/// A system column is read-only too — hence the base class.
[<AttributeUsage(AttributeTargets.Property
                 ||| AttributeTargets.Field)>]
type SystemColumnAttribute() =
    inherit ReadOnlyColumnAttribute()

    override _.Refusal =
        "it is a system column, and PostgreSQL refuses to assign to one at all — `DEFAULT` included. Remove the `set`"
