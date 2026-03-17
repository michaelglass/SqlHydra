module Npgsql.DB

#if NET8_0
open Npgsql.AdventureWorksNet8
#endif
#if NET9_0
open Npgsql.AdventureWorksNet9
#endif
#if NET10_0
open Npgsql.AdventureWorksNet10
#endif

#if DOCKERHOST // devcontainer
let connectionString = @"Server=npgsql;Port=5432;Database=Adventureworks;User Id=postgres;Password=postgres;Timeout=3"
#else
let connectionString = @"Server=localhost;Port=54320;Database=Adventureworks;User Id=postgres;Password=postgres;Timeout=3"
#endif

let toSql (query: SqlHydra.Query.SelectQuery) =
    let compiler = SqlKata.Compilers.PostgresCompiler()
    let kataQuery = query.ToKataQuery()
    let compiled = compiler.Compile(kataQuery)
    // Apply PostgreSQL DISTINCT ON if present
    match SqlHydra.Query.DistinctOnStore.tryTake kataQuery with
    | Some columns ->
        let distinctOnCsv = columns |> String.concat ", "
        let idx = compiled.Sql.IndexOf("SELECT ")
        if idx >= 0 then
            compiled.Sql <- compiled.Sql.Insert(idx + 7, $"DISTINCT ON ({distinctOnCsv}) ")
    | None -> ()
    #if DEBUG
    printfn "toSql: %s" compiled.Sql
    #endif
    compiled.Sql
