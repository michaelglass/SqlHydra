// Positive probe: the escape hatch under design B. Expected to COMPILE.
module Probe
open SqlHydra.Query
open Spike.Support
let q () =
    update {
        for i in Spike.SchemaB.spike do
        set i.tax { Value = 99m }
        where (i.code = "abc")
    }
