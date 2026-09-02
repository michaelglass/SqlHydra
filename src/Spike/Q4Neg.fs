module Spike.Q4Neg
open SqlHydra.Query
open SqlHydra.Query.NpgsqlExtensions
open Spike.Fixture

let q () =
    insert {
        for r in rows do
        writeEntity row
        onConflict r.code
        doUpdateWrite (fun (w: sales.invoice_write) -> w.tax)
    }
