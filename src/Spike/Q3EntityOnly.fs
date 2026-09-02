module Spike.Q3EntityOnly
open SqlHydra.Query
open SqlHydra.Query.NpgsqlExtensions
open Spike.Fixture

// No writeEntity to pin 'Write: what does an unannotated doUpdateWrite do?
let q () =
    insert {
        for r in rows do
        entity { id = 0; code = "a"; price = 10m; tax = 1m }
        onConflict r.code
        doUpdateWrite (fun w -> w.price)
    }
