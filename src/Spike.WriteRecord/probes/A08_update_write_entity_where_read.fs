module Probe
open SqlHydra.Query
open Spike.Schema
let q () =
    update {
        for i in invoices do
        entity writeRow
        where (i.price > 1m)
    }
