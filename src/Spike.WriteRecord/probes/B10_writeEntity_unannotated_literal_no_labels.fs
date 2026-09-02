module Probe
open SqlHydra.Query
open Spike.Schema
// The write record's type is a constrained type variable, so a literal cannot take its type from it.
let q () =
    insert {
        into invoices
        writeEntity { price = 10m; code = "lit" }
    }
