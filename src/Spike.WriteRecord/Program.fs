module Spike.Program

open SqlHydra.Query
open SqlHydra.Query.NpgsqlExtensions
open Spike.Support
open Spike.Schema

let hdr (s: string) = printfn "\n========== %s ==========" s
let ok (s: string) = printfn "  [ok]   %s" s
let bad (s: string) = printfn "  [FAIL] %s" s

let rec describeExn (ex: exn) =
    if isNull ex.InnerException then sprintf "%s: %s" (ex.GetType().Name) (ex.Message.Replace("\n", " "))
    else sprintf "%s: %s  <-- %s" (ex.GetType().Name) (ex.Message.Replace("\n", " ")) (describeExn ex.InnerException)

let tryIt name (f: unit -> unit) =
    try
        f ()
        ok name
    with e -> bad (sprintf "%s -> %s" name (describeExn e))

/// The database-side rejection we expect when a read-only column reaches PostgreSQL by a path we do not guard.
let expectDbRejection name (f: unit -> unit) =
    try
        f ()
        bad (sprintf "%s -> the database accepted it" name)
    with e -> ok (sprintf "%s -> rejected: %s" name (describeExn e))

let showRow (r: sales.spike_invoice) =
    printfn "         id=%d price=%M code=%s tax=%M label=%s dear=%b disc=%A" r.id r.price r.code r.tax r.label r.dear r.disc

let readAll (ctx: QueryContext) =
    select {
        for i in invoices do
        select i
        orderBy i.id
    }
    |> ctx.Select
    |> Seq.toList

let insertSql (q: InsertQuery<_, _>) = (emitter.EmitInsert q.IR).Sql
let updateSql (q: UpdateQuery<_, _>) = (emitter.EmitUpdate q.IR).Sql

[<EntryPoint>]
let main _ =
    hdr "0. What the spec carries at runtime"
    let viaWrite = insert { into invoices; entity writeRow }
    let viaRead = insert { into invoices; entity readRow }
    printfn "         entity writeRow -> spec.Entities.Head : %s" (viaWrite.Spec.Entities.Head.GetType().Name)
    printfn "         entity readRow  -> spec.Entities.Head : %s" (viaRead.Spec.Entities.Head.GetType().Name)
    printfn "         [<ReadOnlyColumn>] fields on the write record: %d" (
        FSharp.Reflection.FSharpType.GetRecordFields(typeof<sales.spike_invoice_write>)
        |> Array.filter (fun p -> System.Attribute.IsDefined(p, typeof<SqlHydra.ReadOnlyColumnAttribute>))
        |> Array.length)

    use ctx = openCtx ()
    exec ctx ddl

    hdr "1. INSERT via the write record: column list is the write record's fields"
    tryIt "insert entity writeRow" (fun () ->
        let n = viaWrite |> ctx.Insert
        printfn "         rows inserted = %d" n)

    hdr "2. Read back as the read record: every generated column hydrates, including the NULLable one"
    tryIt "select i" (fun () ->
        let rows = readAll ctx
        printfn "         row type = %s" (rows.Head.GetType().Name)
        rows |> List.iter showRow)

    hdr "3. UPDATE via the write record, filtered on the read record"
    tryIt "update entity writeRow where i.code" (fun () ->
        let n =
            update {
                for i in invoices do
                entity { writeRow with price = 33m }
                where (i.code = "abc")
            }
            |> ctx.Update
        printfn "         rows updated = %d" n)

    hdr "4. Read again: the database recomputed the generated columns"
    tryIt "select i" (fun () -> readAll ctx |> List.iter showRow)

    hdr "5. The read-record path is unchanged: `entity readRow` still relies on the runtime filter"
    tryIt "insert entity readRow (id=999, tax=999 never sent)" (fun () ->
        let n = insert { into invoices; entity { readRow with code = "via-read" } } |> ctx.Insert
        printfn "         rows inserted = %d" n)

    hdr "6. `set` on a read-only column: the accepted escape hatch, still caught at runtime"
    expectDbRejection "set i.tax 5m" (fun () ->
        update {
            for i in invoices do
            set i.tax 5m
            where (i.code = "abc")
        }
        |> ctx.Update
        |> ignore)

    hdr "7. includeColumn / excludeColumn selectors are read-typed; they intersect with the write record"
    tryIt "includeColumn i.tax with entity writeRow -> SQL" (fun () ->
        let q =
            insert {
                for i in invoices do
                entity writeRow
                includeColumn i.tax
            }
        printfn "         %s" (insertSql q))
    tryIt "includeColumn i.price with entity writeRow -> SQL" (fun () ->
        let q =
            insert {
                for i in invoices do
                entity writeRow
                includeColumn i.price
            }
        printfn "         %s" (insertSql q))
    tryIt "excludeColumn i.code with entity writeRow -> SQL" (fun () ->
        let q =
            insert {
                for i in invoices do
                entity writeRow
                excludeColumn i.code
            }
        printfn "         %s" (insertSql q))

    hdr "8. getId with the write record: RETURNING reads the read-only identity"
    tryIt "getId i.id" (fun () ->
        let id =
            insert {
                for i in invoices do
                entity { writeRow with code = "with-id" }
                getId i.id
            }
            |> ctx.Insert
        printfn "         returned id = %d" id)

    hdr "9. onConflict/doUpdate selectors are read-typed: a read-only column can reach a SET clause"
    let upsertTax =
        insert {
            for i in invoices do
            entity writeRow
            onConflict i.code
            doUpdate i.tax
        }
    tryIt "doUpdate i.tax -> SQL" (fun () -> printfn "         %s" (insertSql upsertTax))
    expectDbRejection "doUpdate i.tax executed" (fun () -> upsertTax |> ctx.Insert |> ignore)
    tryIt "doUpdate i.price executed (the proper upsert)" (fun () ->
        let n =
            insert {
                for i in invoices do
                entity { writeRow with price = 44m }
                onConflict i.code
                doUpdate i.price
            }
            |> ctx.Insert
        printfn "         rows = %d" n
        readAll ctx |> List.filter (fun r -> r.code = "abc") |> List.iter showRow)

    hdr "10. entities with a list of write records"
    tryIt "entities [w1; w2]" (fun () ->
        let q =
            insert {
                into invoices
                entities [ { writeRow with code = "m1" }; { writeRow with code = "m2" } ]
            }
        printfn "         %s" (insertSql q)
        printfn "         rows inserted = %d" (ctx.Insert q))

    hdr "11. update entity writeRow narrowed by includeColumn"
    tryIt "includeColumn i.price -> SQL" (fun () ->
        let q =
            update {
                for i in invoices do
                entity { writeRow with price = 1m }
                includeColumn i.price
                where (i.code = "m1")
            }
        printfn "         %s" (updateSql q))

    hdr "12. Final state"
    tryIt "select i" (fun () -> readAll ctx |> List.iter showRow)

    exec ctx dropDdl
    printfn "\ndone."
    0
