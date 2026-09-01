namespace SqlHydra

open System

/// Marks a generated field as a column of a view that maps onto no column of a base
/// relation. PostgreSQL [auto-updates a view](https://www.postgresql.org/docs/current/sql-createview.html#SQL-CREATEVIEW-UPDATABLE-VIEWS)
/// only where a column is a plain reference to one, so an expression or aggregate column
/// takes no write. Read-only too — hence the base class.
[<AttributeUsage(AttributeTargets.Property
                 ||| AttributeTargets.Field)>]
type UnwritableViewColumnAttribute() =
    inherit ReadOnlyColumnAttribute()

    override _.Refusal =
        "the view maps it onto no base-table column, so no write reaches it — `DEFAULT` included. Remove the `set`, or write to the relation underneath the view"
