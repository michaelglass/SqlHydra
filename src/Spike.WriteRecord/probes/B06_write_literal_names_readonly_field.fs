module Probe
open SqlHydra.Query
open Spike.Schema
let w : sales.spike_invoice_write = { price = 10m; code = "x"; tax = 5m }
