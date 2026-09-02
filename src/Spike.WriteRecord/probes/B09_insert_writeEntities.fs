module Probe
open SqlHydra.Query
open Spike.Schema
let q () =
    insert {
        into invoices
        writeEntities [ writeRow; { writeRow with code = "def" } ]
    }
