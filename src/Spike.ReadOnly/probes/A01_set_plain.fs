module Probe
open SqlHydra.Query
open Spike.Support
let q =
    update {
        for i in Spike.SchemaA.spike do
        set i.tax 99m
        where (i.code = "abc")
    }
