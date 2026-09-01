// Positive probe: the escape hatch. Expected to COMPILE.
module Probe
open SqlHydra.Query
open Spike.Support
let q () =
    update {
        for i in Spike.SchemaA.spike do
        set i.tax 99m<ro>
        where (i.code = "abc")
    }
