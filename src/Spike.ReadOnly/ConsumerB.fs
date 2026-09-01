/// The same application code — DESIGN B.
module Spike.ConsumerB

open SqlHydra.Query
open Spike.Support
open Spike.SchemaB

let formatMoney (amount: decimal) = sprintf "$%.2f" amount

// --- reads -----------------------------------------------------------------

let totalTax (rows: sales.spike_readonly list) : decimal =
    rows |> List.sumBy (fun r -> r.tax.Value)

let grandTotal (r: sales.spike_readonly) : decimal =
    r.price + r.tax.Value

let describe (r: sales.spike_readonly) =
    sprintf "%s: price %s, tax %s" r.code (formatMoney r.price) (formatMoney r.tax.Value)

let isTaxed (r: sales.spike_readonly) = r.tax.Value > 0m

/// Unlike design A this also covers text and bool.
let banner (r: sales.spike_readonly) =
    if r.dear.Value then r.label.Value.ToUpper() else r.label.Value

let discountOrZero (r: sales.spike_readonly) : decimal =
    r.disc |> Option.map _.Value |> Option.defaultValue 0m

let logRow (r: sales.spike_readonly) = printfn "tax=%M disc=%A" r.tax.Value r.disc

// --- queries ---------------------------------------------------------------

/// The `where` works only because of the NProperty change in LinqExpressionVisitors;
/// without it this raises NotImplementedException at query-build time.
let expensive () =
    select {
        for i in spike do
        where (i.tax.Value > 1m)
        select i
    }

/// orderBy has its own visitor, which the NProperty change does NOT cover.
/// This compiles, builds, and emits ORDER BY "i"."Value" — a column that does not exist.
let expensiveOrdered () =
    select {
        for i in spike do
        where (i.tax.Value > 1m)
        orderByDescending i.tax.Value
        select i
    }

let taxes () =
    select {
        for i in spike do
        select i.tax
    }

// --- writes ----------------------------------------------------------------

let newRow code price : sales.spike_readonly =
    { id = { Value = 0 }
      seq = 0
      price = price
      code = code
      tax = { Value = 0m }
      label = { Value = "" }
      dear = { Value = false }
      disc = None }
