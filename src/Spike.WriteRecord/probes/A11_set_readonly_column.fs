module Probe
open SqlHydra.Query
open Spike.Schema
let q () =
    update {
        for i in invoices do
        set i.tax 5m
        where (i.code = "abc")
    }
