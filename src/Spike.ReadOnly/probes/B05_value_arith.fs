// Positive probe: reading under design B. Expected to COMPILE.
module Probe
open Spike.Support
let f (row: Spike.SchemaB.sales.spike_readonly) =
    printfn "%M" (row.tax.Value * 2m)
    printfn "%s" row.label.Value
