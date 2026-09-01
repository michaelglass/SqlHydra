module Spike.Program

open System
open SqlHydra.Query
open Spike.Support

let hdr (s: string) = printfn "\n========== %s ==========" s
let ok (s: string) = printfn "  [ok]   %s" s
let bad (s: string) = printfn "  [FAIL] %s" s

let tryIt name (f: unit -> unit) =
    try
        f ()
        ok name
    with e ->
        let rec chain (ex: exn) =
            if isNull ex.InnerException then sprintf "%s: %s" (ex.GetType().Name) (ex.Message.Replace("\n", " "))
            else sprintf "%s: %s  <-- %s" (ex.GetType().Name) (ex.Message.Replace("\n", " ")) (chain ex.InnerException)
        bad (sprintf "%s -> %s" name (chain e))

// ---------------------------------------------------------------------------
[<EntryPoint>]
let main _ =
    hdr "0. Runtime erasure of units of measure"
    let fieldsA = FSharp.Reflection.FSharpType.GetRecordFields(typeof<SchemaA.sales.spike_readonly>)
    for f in fieldsA do
        printfn "  A  %-8s -> %s" f.Name f.PropertyType.FullName
    let fieldsB = FSharp.Reflection.FSharpType.GetRecordFields(typeof<SchemaB.sales.spike_readonly>)
    for f in fieldsB do
        printfn "  B  %-8s -> %s" f.Name f.PropertyType.FullName

    use ctx = openCtx ()
    exec ctx ddl

    // -----------------------------------------------------------------------
    hdr "1. Design A: entity insert (read-only cols filtered by attribute)"
    tryIt "insert entity" (fun () ->
        let n =
            insert {
                into SchemaA.spike
                entity SchemaA.sampleRow
            }
            |> ctx.Insert
        printfn "         rows inserted = %d" n)

    hdr "2. Design A: hydration round-trip"
    let mutable rowA = Unchecked.defaultof<SchemaA.sales.spike_readonly>
    tryIt "select whole entity" (fun () ->
        let rows =
            select {
                for i in SchemaA.spike do
                select i
            }
            |> ctx.Select
            |> Seq.toList
        rowA <- rows |> List.exactlyOne
        printfn "         id=%A seq=%d price=%M code=%s tax=%A label=%s dear=%b disc=%A"
            rowA.id rowA.seq rowA.price rowA.code rowA.tax rowA.label rowA.dear rowA.disc)

    hdr "3. Design A: consumer code on the read side"
    // arithmetic: multiplication by a dimensionless literal keeps the measure
    tryIt "row.tax * 2m" (fun () -> printfn "         %A" (rowA.tax * 2m))
    // addition needs a measure-carrying literal
    tryIt "row.tax + 2m<ro>" (fun () -> printfn "         %A" (rowA.tax + 2m<ro>))
    // stripping the measure to hand it to ordinary code
    tryIt "decimal-strip" (fun () ->
        let plain: decimal = decimal rowA.tax
        printfn "         %M" plain)
    tryIt "string rowA.tax" (fun () -> printfn "         %s" (string rowA.tax))
    tryIt "sprintf %%A" (fun () -> printfn "         %s" (sprintf "%A" rowA.tax))

    hdr "4. Design A: scalar select of a measure column"
    tryIt "select i.tax" (fun () ->
        let taxes =
            select {
                for i in SchemaA.spike do
                select i.tax
            }
            |> ctx.Select
            |> Seq.toList
        printfn "         %A" taxes)

    hdr "5. Design A: where on a measure column (needs a measure literal)"
    tryIt "where (i.tax > 0m<ro>)" (fun () ->
        let rows =
            select {
                for i in SchemaA.spike do
                where (i.tax > 0m<ro>)
                select i.price
            }
            |> ctx.Select
            |> Seq.toList
        printfn "         %A" rows)

    hdr "6. Design A: entity update skips the read-only columns"
    tryIt "update entity" (fun () ->
        let n =
            update {
                for i in SchemaA.spike do
                entity { rowA with price = 20m }
                where (i.code = "abc")
            }
            |> ctx.Update
        printfn "         rows updated = %d" n)
    tryIt "re-read after update" (fun () ->
        let r =
            select {
                for i in SchemaA.spike do
                select i
            }
            |> ctx.Select
            |> Seq.exactlyOne
        printfn "         price=%M tax=%A label=%s dear=%b" r.price r.tax r.label r.dear)

    hdr "7. Design A: the runtime raise is still the only guard for text/bool"
    tryIt "set i.label \"nope\" (expect runtime raise)" (fun () ->
        update {
            for i in SchemaA.spike do
            set i.label "nope"
            where (i.code = "abc")
        }
        |> ctx.Update
        |> ignore)

    // -----------------------------------------------------------------------
    hdr "8. Design B: hydration of a wrapper-typed field, unmodified SqlHydra"
    tryIt "select whole entity (B)" (fun () ->
        let rows =
            select {
                for i in SchemaB.spike do
                select i
            }
            |> ctx.Select
            |> Seq.toList
        printfn "         %A" rows)

    hdr "9. Design B: scalar select of a wrapper column"
    tryIt "select i.tax (B)" (fun () ->
        let rows =
            select {
                for i in SchemaB.spike do
                select i.tax
            }
            |> ctx.Select
            |> Seq.toList
        printfn "         %A" rows)

    hdr "10. Design B: SQL emission for a where on the wrapped value"
    tryIt "where (i.tax.Value > 0m) -> SQL" (fun () ->
        let sql =
            select {
                for i in SchemaB.spike do
                where (i.tax.Value > 0m)
                select i.price
            }
            |> toSql
        printfn "         %s" sql)

    hdr "11. Design B: whole-entity SELECT sql"
    tryIt "select i (B) -> SQL" (fun () ->
        let sql =
            select {
                for i in SchemaB.spike do
                select i
            }
            |> toSql
        printfn "         %s" sql)

    hdr "12. Design B: insert entity (read-only cols filtered, so wrapper never reaches a param)"
    tryIt "insert entity (B)" (fun () ->
        let n =
            insert {
                into SchemaB.spike
                entity SchemaB.sampleRow
            }
            |> ctx.Insert
        printfn "         rows inserted = %d" n)

    hdr "13. Consumer code, run for real"
    tryIt "ConsumerA" (fun () ->
        let rows = ConsumerA.expensive () |> ctx.Select |> Seq.toList
        printfn "         totalTax   = %M" (ConsumerA.totalTax rows)
        printfn "         grandTotal = %M" (ConsumerA.grandTotal rows.Head)
        printfn "         describe   = %s" (ConsumerA.describe rows.Head)
        printfn "         isTaxed    = %b" (ConsumerA.isTaxed rows.Head)
        printfn "         discount   = %M" (ConsumerA.discountOrZero rows.Head)
        ConsumerA.logRow rows.Head
        printfn "         taxes      = %A" (ConsumerA.taxes () |> ctx.Select |> Seq.toList))

    tryIt "ConsumerB orderBy (one visitor fixed, the rest silently wrong)" (fun () ->
        printfn "         %s" (ConsumerB.expensiveOrdered () |> toSql))

    tryIt "ConsumerB" (fun () ->
        let rows = ConsumerB.expensive () |> ctx.Select |> Seq.toList
        printfn "         totalTax   = %M" (ConsumerB.totalTax rows)
        printfn "         grandTotal = %M" (ConsumerB.grandTotal rows.Head)
        printfn "         describe   = %s" (ConsumerB.describe rows.Head)
        printfn "         banner     = %s" (ConsumerB.banner rows.Head)
        printfn "         discount   = %M" (ConsumerB.discountOrZero rows.Head)
        ConsumerB.logRow rows.Head
        printfn "         taxes      = %A" (ConsumerB.taxes () |> ctx.Select |> Seq.toList))

    exec ctx dropDdl
    printfn "\ndone."
    0
