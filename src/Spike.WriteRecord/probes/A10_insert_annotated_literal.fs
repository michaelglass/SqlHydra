module Probe
open SqlHydra.Query
open Spike.Schema
let q () =
    insert {
        into invoices
        entity ({ price = 10m; code = "lit" } : sales.spike_invoice_write)
    }
