module Probe
open SqlHydra.Query
open Spike.Schema
open Spike.Schema.sales
// Schema.fs declares spike_invoice_write after spike_invoice, as the generator would.
let q () =
    insert {
        into invoices
        writeEntity { price = 10m; code = "lit" }
    }
