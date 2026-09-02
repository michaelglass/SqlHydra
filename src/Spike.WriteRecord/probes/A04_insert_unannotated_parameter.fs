module Probe
open SqlHydra.Query
open Spike.Schema
// Mirrors Tests/Npgsql/QueryIntegrationTests.fs:600 `let upsertCurrency currency = ...`.
let insertIt currency =
    insert {
        into invoices
        entity currency
    }
