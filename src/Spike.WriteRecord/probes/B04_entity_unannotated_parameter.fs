module Probe
open SqlHydra.Query
open Spike.Schema
let insertIt currency =
    insert {
        into invoices
        entity currency
    }
