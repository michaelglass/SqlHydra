/// Design A — units of measure. What codegen would have to emit.
module Spike.SchemaA

open SqlHydra.Query
open Spike.Support

module sales =
    [<CLIMutable>]
    type spike_readonly =
        { [<SqlHydra.ReadOnlyColumn>]
          id: int<ro>
          seq: int
          price: decimal
          code: string
          [<SqlHydra.ReadOnlyColumn>]
          tax: decimal<ro>
          // text: no measure exists for string, so this stays writable at compile time.
          [<SqlHydra.ReadOnlyColumn>]
          label: string
          // bool: same.
          [<SqlHydra.ReadOnlyColumn>]
          dear: bool
          // a NULLable generated numeric: does the measure survive the Option?
          [<SqlHydra.ReadOnlyColumn>]
          disc: decimal<ro> option }

let spike = table<sales.spike_readonly>

/// The record a consumer must build to insert. Note every read-only numeric field
/// needs a measure-carrying literal.
let sampleRow: sales.spike_readonly =
    { id = 0<ro>
      seq = 7
      price = 10m
      code = "abc"
      tax = 0m<ro>
      label = ""
      dear = false
      disc = None }
