module Spike.Support

open SqlHydra.Query
open Npgsql

let connectionString =
    @"Server=localhost;Port=54320;Database=Adventureworks;User Id=postgres;Password=postgres;Timeout=5"

let emitter = PostgresEmitter() :> ISqlEmitter

let openCtx () =
    let conn = new NpgsqlConnection(connectionString)
    conn.Open()
    let ctx = new QueryContext(conn, emitter)
    ctx.Logger <- (fun cq -> printfn "         SQL: %s" cq.Sql)
    ctx

let exec (ctx: QueryContext) (sql: string) =
    use cmd = ctx.Connection.CreateCommand()
    cmd.CommandText <- sql
    cmd.ExecuteNonQuery() |> ignore

/// The probe table the lead asked for: an identity, one writable column plus a unique key,
/// and generated columns of every shape that hid a bug last time: numeric, text, bool, NULLable numeric.
let ddl =
    """
    DROP TABLE IF EXISTS sales.spike_invoice;
    CREATE TABLE sales.spike_invoice (
        id     int GENERATED ALWAYS AS IDENTITY,
        price  numeric NOT NULL,
        code   text NOT NULL UNIQUE,
        tax    numeric GENERATED ALWAYS AS (price * 0.1) STORED,
        label  text GENERATED ALWAYS AS (code || '-gen') STORED,
        dear   boolean GENERATED ALWAYS AS (price > 5) STORED,
        disc   numeric GENERATED ALWAYS AS (nullif(price, 20) * 0.05) STORED
    );
    """

let dropDdl = "DROP TABLE IF EXISTS sales.spike_invoice;"
