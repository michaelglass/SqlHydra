/// Design B — nominal wrapper. What codegen would have to emit.
module Spike.SchemaB

open SqlHydra.Query
open Spike.Support

module sales =
    [<CLIMutable>]
    type spike_readonly =
        { [<SqlHydra.ReadOnlyColumn>]
          id: ReadOnly<int>
          seq: int
          price: decimal
          code: string
          [<SqlHydra.ReadOnlyColumn>]
          tax: ReadOnly<decimal>
          [<SqlHydra.ReadOnlyColumn>]
          label: ReadOnly<string>
          [<SqlHydra.ReadOnlyColumn>]
          dear: ReadOnly<bool>
          [<SqlHydra.ReadOnlyColumn>]
          disc: ReadOnly<decimal> option }

let spike = table<sales.spike_readonly>

let sampleRow: sales.spike_readonly =
    { id = { Value = 0 }
      seq = 7
      price = 10m
      code = "abc"
      tax = { Value = 0m }
      label = { Value = "" }
      dear = { Value = false }
      disc = None }
