module Npgsql.``Regression Tests``

open System
open Swensen.Unquote
open SqlHydra.Query
open SqlHydra.Query.NpgsqlExtensions
open NUnit.Framework
open Npgsql.DB
#if NET8_0
open Npgsql.AdventureWorksNet8
#endif
#if NET9_0
open Npgsql.AdventureWorksNet9
#endif
#if NET10_0
open Npgsql.AdventureWorksNet10
#endif

// ============================================================
// Manually-defined types matching test tables (no codegen needed)
// ============================================================
module ``public`` =
    [<CLIMutable>]
    type test_emails = {
        id: Guid
        source_id: Guid option
        user_id: Guid option
        sender: string
        subject: string option
        status: string
        owner_type: string
        received_at: DateTime
        chunk_status: string option
        simhash: int64 option
        verified_at: DateTime option
    }

    let test_emails = table<test_emails>

    [<CLIMutable>]
    type test_sources = {
        id: Guid
        name: string
        verified_at: DateTime option
        priority: int
    }

    let test_sources = table<test_sources>

    [<CLIMutable>]
    type test_articles = {
        id: Guid
        email_id: Guid
        title: string
    }

    let test_articles = table<test_articles>

    [<CLIMutable>]
    type test_preferences = {
        id: Guid
        source_id: Guid
        user_id: Guid
        priority: int
    }

    let test_preferences = table<test_preferences>

    [<CLIMutable>]
    type test_events = {
        id: Guid
        email_event_id: string option
        user_id: Guid
        event_type: string
        created_at: DateTime
    }

    let test_events = table<test_events>

open ``public``

// ============================================================
// Database connection (reuse from DB.fs)
// ============================================================
let db =
    let dataSource = Npgsql.NpgsqlDataSourceBuilder(connectionString).Build()
    QueryContextFactory.Create(dataSource, sqlLogger = printf "SQL: %O")

// ============================================================
// Test setup: create tables via raw SQL
// ============================================================
let private createTablesSql = """
CREATE TABLE IF NOT EXISTS public.test_sources (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    name VARCHAR(255) NOT NULL,
    verified_at TIMESTAMPTZ,
    priority INT NOT NULL DEFAULT 5
);

CREATE TABLE IF NOT EXISTS public.test_emails (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    source_id UUID REFERENCES public.test_sources(id),
    user_id UUID,
    sender VARCHAR(255) NOT NULL,
    subject VARCHAR(255),
    status VARCHAR(50) NOT NULL DEFAULT 'stored',
    owner_type VARCHAR(50) NOT NULL DEFAULT 'user',
    received_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    chunk_status VARCHAR(50),
    simhash BIGINT,
    verified_at TIMESTAMPTZ
);

CREATE TABLE IF NOT EXISTS public.test_articles (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    email_id UUID NOT NULL REFERENCES public.test_emails(id),
    title VARCHAR(255) NOT NULL
);

CREATE TABLE IF NOT EXISTS public.test_preferences (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    source_id UUID NOT NULL REFERENCES public.test_sources(id),
    user_id UUID NOT NULL,
    priority INT NOT NULL DEFAULT 5
);

CREATE TABLE IF NOT EXISTS public.test_events (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    email_event_id VARCHAR(255),
    user_id UUID NOT NULL,
    event_type VARCHAR(50) NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE UNIQUE INDEX IF NOT EXISTS idx_test_events_email_event_id
ON public.test_events (email_event_id) WHERE email_event_id IS NOT NULL;
"""

[<OneTimeSetUp>]
let setup () = task {
    use! shared = db.OpenContextAsync()
    use cmd = shared.Connection.CreateCommand()
    cmd.CommandText <- createTablesSql
    cmd.Transaction <- shared.Transaction |> Option.defaultValue null
    cmd.ExecuteNonQuery() |> ignore
}

// Placeholder test to verify setup works
[<Test>]
let ``Setup - test tables exist``() = task {
    use! shared = db.OpenContextAsync()
    use cmd = shared.Connection.CreateCommand()
    cmd.CommandText <- "SELECT COUNT(*) FROM information_schema.tables WHERE table_name = 'test_sources' AND table_schema = 'public'"
    cmd.Transaction <- shared.Transaction |> Option.defaultValue null
    let! count = cmd.ExecuteScalarAsync()
    Assert.AreEqual(1L, count)
}
