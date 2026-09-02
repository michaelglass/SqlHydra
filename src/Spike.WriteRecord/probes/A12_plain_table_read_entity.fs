module Probe
open SqlHydra.Query
open Spike.Schema
let row : sales.plain = { id = 1; name = "n" }
let q () =
    insert {
        into plains
        entity row
    }
