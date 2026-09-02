module Probe
open SqlHydra.Query
open Spike.Schema
let w : sales.other_write = { qty = 1 }
let q () =
    insert {
        into invoices
        writeEntity w
    }
