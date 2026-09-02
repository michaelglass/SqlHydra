module Probe
open SqlHydra.Query
open Spike.Schema
open Spike.Schema.sales
/// A caller's own record sharing a label, declared after the generated module was opened.
type decoy = { price: decimal; note: string }
let q () =
    insert {
        into invoices
        writeEntity { price = 10m; code = "lit" }
    }
