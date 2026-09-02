module Probe
open SqlHydra.Query
open Spike.Schema
let q () =
    update {
        for i in invoices do
        writeEntity writeRow
        where (i.price > 1m)
    }
