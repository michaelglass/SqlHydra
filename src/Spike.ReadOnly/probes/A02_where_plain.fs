module Probe
open SqlHydra.Query
open Spike.Support
let q =
    select {
        for i in Spike.SchemaA.spike do
        where (i.tax > 5m)
        select i.price
    }
