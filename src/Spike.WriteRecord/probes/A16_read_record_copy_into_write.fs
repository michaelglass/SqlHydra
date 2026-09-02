module Probe
open SqlHydra.Query
open Spike.Schema
// A copy-and-update of the read record stays the read record: there is no implicit narrowing.
let q () =
    insert {
        into invoices
        entity ({ readRow with price = 5m } : sales.spike_invoice_write)
    }
