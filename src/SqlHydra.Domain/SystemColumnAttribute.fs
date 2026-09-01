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
