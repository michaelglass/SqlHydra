namespace SqlHydra

open System

/// Marks a field the database writes: SqlHydra.Query leaves it out of every INSERT column
/// list and UPDATE SET clause. https://www.postgresql.org/docs/current/ddl-generated-columns.html
[<AttributeUsage(AttributeTargets.Property
                 ||| AttributeTargets.Field)>]
type ReadOnlyColumnAttribute() =
    inherit Attribute()

/// SPIKE (design B): the nominal wrapper codegen would emit for a read-only column.
/// Lives in SqlHydra.Domain so both the generator and SqlHydra.Query.Hydration can see it.
type ReadOnly<'T> =
    { Value: 'T }
    override this.ToString() = sprintf "%A" this.Value
