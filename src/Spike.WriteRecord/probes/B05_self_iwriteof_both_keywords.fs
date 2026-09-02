module Probe
open SqlHydra.Query
open Spike.Schema
let row : sales.selfwrite = { name = "me" }
let viaEntity () =
    insert {
        into selfwrites
        entity row
    }
let viaWriteEntity () =
    insert {
        into selfwrites
        writeEntity row
    }
