module Probe
open SqlHydra.Query
open Spike.Schema
open Spike.Schema.sales
// Labels in scope, no annotation: which record does F# pick for { price; code }?
let q () =
    insert {
        into invoices
        entity { price = 10m; code = "lit" }
    }
