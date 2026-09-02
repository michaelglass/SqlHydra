module Probe
open SqlHydra.Query
open Spike.Schema
let q () =
    insert {
        for i in invoices do
        writeEntity writeRow
        getId i.id
    }
