namespace SqlHydra

open System

/// Marks a generated field as one whose value the database owns.
///
/// SqlHydra.Query keeps a field carrying this attribute out of every INSERT column list and
/// UPDATE SET clause, so the record can hold the field -- and must, since a record has no
/// optional fields -- without whatever it holds ever reaching the statement. Reads are
/// untouched.
///
/// Emitted from `Column.IsReadOnly`. A PostgreSQL system column such as `xmin`, a SQL Server
/// `rowversion` and a generated column are all the same shape.
[<AttributeUsage(AttributeTargets.Property
                 ||| AttributeTargets.Field)>]
type ReadOnlyColumnAttribute() =
    inherit Attribute()
