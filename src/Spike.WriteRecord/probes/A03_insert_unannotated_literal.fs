module Probe
open SqlHydra.Query
open Spike.Schema
// Labels are not in scope (module `sales` is not opened): the expected type had to come from `entity`.
let q () =
    insert {
        into invoices
        entity { price = 10m; code = "lit" }
    }
