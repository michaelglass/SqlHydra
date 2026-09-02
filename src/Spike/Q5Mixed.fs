module Spike.Q5Mixed
open SqlHydra.Query
open SqlHydra.Query.NpgsqlExtensions
open Spike.Fixture

let q () =
    insert {
        for r in rows do
        writeEntity row
        onConflictDoUpdateWrite r.code (fun w -> w.price)
    }
