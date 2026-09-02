module Probe
open SqlHydra.Query
open Spike.Schema
// Labels not in scope: the expected type must come from `entity`, as it does today.
let q () =
    insert {
        into invoices
        entity { id = 0; price = 10m; code = "lit"; tax = 0m; label = ""; dear = false; disc = None }
    }
