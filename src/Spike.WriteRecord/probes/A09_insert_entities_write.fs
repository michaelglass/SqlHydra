module Probe
open SqlHydra.Query
open Spike.Schema
let q () =
    insert {
        into invoices
        entities [ writeRow; { writeRow with code = "def" } ]
    }
