module Probe
open Spike.Support
let f (row: Spike.SchemaB.sales.spike_readonly) = row.tax * 2m
