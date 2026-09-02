module Probe
open SqlHydra.Query
open SqlHydra.Query.NpgsqlExtensions
open Spike.Schema
let q () =
    insert {
        for i in invoices do
        entity writeRow
        onConflict i.code
        doUpdate i.tax
    }
