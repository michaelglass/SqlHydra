module Probe
open Spike.Support
let applyRate (amount: decimal) = amount * 1.05m
let f (row: Spike.SchemaA.sales.spike_readonly) = applyRate row.tax
