/// Ordinary application code against a table with read-only columns — DESIGN A.
/// Every `decimal ...` / `<ro>` in here is cost the design imposes; the same code
/// against today's plain `decimal` fields needs none of it.
module Spike.ConsumerA

open SqlHydra.Query
open Spike.Support
open Spike.SchemaA

/// A pre-existing domain helper. It knows nothing about SqlHydra and takes a plain decimal.
/// This is the shape most real code has, and it is the reason for every strip below.
let formatMoney (amount: decimal) = sprintf "$%.2f" amount

let netOf (gross: decimal) (rate: decimal) = gross * (1m - rate)

// --- reads -----------------------------------------------------------------

/// A total over a read-only column. Measures survive summation, so the *result*
/// is decimal<ro> and has to be stripped before it can leave.
let totalTax (rows: sales.spike_readonly list) : decimal =
    rows |> List.sumBy (fun r -> r.tax) |> decimal

/// Mixing a read-only column with a writable one. `+` demands matching measures,
/// so one side must be stripped or re-tagged at every site.
let grandTotal (r: sales.spike_readonly) : decimal =
    r.price + decimal r.tax

/// Handing the value to pre-existing code: a strip at every call site.
let describe (r: sales.spike_readonly) =
    sprintf "%s: price %s, tax %s" r.code (formatMoney r.price) (formatMoney (decimal r.tax))

/// Comparisons against literals need the literal tagged.
let isTaxed (r: sales.spike_readonly) = r.tax > 0m<ro>

/// A nullable read-only column: the strip happens inside the Option.
let discountOrZero (r: sales.spike_readonly) : decimal =
    r.disc |> Option.map decimal |> Option.defaultValue 0m

/// printf's %M does accept a measure, so plain formatting is unaffected.
let logRow (r: sales.spike_readonly) = printfn "tax=%M disc=%A" r.tax r.disc

// --- queries ---------------------------------------------------------------

/// A where predicate on a read-only column: the literal must carry the measure.
let expensive () =
    select {
        for i in spike do
        where (i.tax > 1m<ro>)
        orderByDescending i.tax
        select i
    }

/// Projecting a read-only column out: comes back as decimal<ro>, so anything
/// downstream that wants a decimal strips it.
let taxes () =
    select {
        for i in spike do
        select i.tax
    }

// --- writes ----------------------------------------------------------------

/// Building a row to insert. The database owns id/tax/disc, but the record still
/// demands a value for each, and each numeric one must carry the measure.
let newRow code price : sales.spike_readonly =
    { id = 0<ro>
      seq = 0
      price = price
      code = code
      tax = 0m<ro>       // ignored on the wire; still has to be written, and tagged
      label = ""
      dear = false
      disc = None }
