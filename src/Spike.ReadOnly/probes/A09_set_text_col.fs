// Positive probe: a generated TEXT column has no measure, so this still COMPILES
// and only raises at runtime.
module Probe
open SqlHydra.Query
open Spike.Support
let q () =
    update {
        for i in Spike.SchemaA.spike do
        set i.label "nope"
        set i.dear true
        where (i.code = "abc")
    }
