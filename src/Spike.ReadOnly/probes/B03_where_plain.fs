module Probe
open SqlHydra.Query
open Spike.Support
let q =
    select {
        for i in Spike.SchemaB.spike do
        where (i.tax > 0m)
        select i.price
    }
