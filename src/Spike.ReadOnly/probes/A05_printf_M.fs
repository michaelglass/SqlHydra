module Probe
open Spike.Support
let f (row: Spike.SchemaA.sales.spike_readonly) = printfn "%M" row.tax
