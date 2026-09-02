/// Hand-written to the shape `SchemaTemplate` would emit (see the sketch in the report).
module Spike.Schema

open SqlHydra
open SqlHydra.Query

module sales =
    [<CLIMutable>]
    type spike_invoice =
        { [<ReadOnlyColumn>]
          id: int
          price: decimal
          code: string
          [<ReadOnlyColumn>]
          tax: decimal
          [<ReadOnlyColumn>]
          label: string
          [<ReadOnlyColumn>]
          dear: bool
          [<ReadOnlyColumn>]
          disc: Option<decimal> }

    /// Emitted only because `spike_invoice` has read-only columns; holds the other columns.
    [<CLIMutable>]
    type spike_invoice_write =
        { price: decimal
          code: string }
        interface IWriteOf<spike_invoice>

    /// A table with no read-only columns: no write record is emitted, the read type is the write type.
    [<CLIMutable>]
    type plain =
        { id: int
          name: string }

    /// The ambiguous case the lead asked about: a type that claims to be its own write record.
    [<CLIMutable>]
    type selfwrite =
        { name: string }
        interface IWriteOf<selfwrite>

    /// A second table with a write record, to show `IWriteOf<'T>` binds the write record to its own table.
    [<CLIMutable>]
    type other =
        { [<ReadOnlyColumn>]
          id: int
          qty: int }

    [<CLIMutable>]
    type other_write =
        { qty: int }
        interface IWriteOf<other>

let invoices = table<sales.spike_invoice>
let plains = table<sales.plain>
let selfwrites = table<sales.selfwrite>
let others = table<sales.other>

let writeRow : sales.spike_invoice_write = { price = 10m; code = "abc" }

let readRow : sales.spike_invoice =
    { id = 999; price = 10m; code = "abc"; tax = 999m; label = "never sent"; dear = false; disc = Some 999m }
