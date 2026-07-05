namespace SqlHydra

open System

[<AttributeUsage(AttributeTargets.Property
                 ||| AttributeTargets.Field)>]
type ProviderDbTypeAttribute(providerDbTypeName: string) =
    inherit Attribute()

    member this.ProviderDbTypeName = providerDbTypeName

/// Marks a generated record field as a read-only, database-managed *system column*
/// (e.g. PostgreSQL's `xmin`/`xmax`). SqlHydra.Query treats these columns as:
///   * SELECTed explicitly by name (they are excluded by `SELECT *` / `alias.*`), so
///     whole-entity reads hydrate them correctly, and
///   * NEVER emitted in INSERT column lists or UPDATE SET clauses (the database owns them),
/// while remaining usable in `where` predicates (e.g. optimistic-concurrency guards).
[<AttributeUsage(AttributeTargets.Property
                 ||| AttributeTargets.Field)>]
type SystemColumnAttribute() =
    inherit Attribute()
