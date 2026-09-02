module Probe
open SqlHydra.Query
open Spike.Schema
let row : sales.selfwrite = { name = "me" }
let q () =
    insert {
        into selfwrites
        entity row
    }
