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

// Helper to insert test data via raw SQL
let private execSql (shared: QueryContext) (sql: string) = task {
    use cmd = shared.Connection.CreateCommand()
    cmd.Transaction <- shared.Transaction |> Option.defaultValue null
    cmd.CommandText <- sql
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

// ============================================================
// Bug 1: join on (optionalCol = Some nonNullCol)
// ============================================================

[<Test>]
let ``Bug1: join on Option column = Some non-nullable PK``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let sourceId = Guid.NewGuid()
    let emailId = Guid.NewGuid()

    do! execSql shared $"INSERT INTO public.test_sources (id, name) VALUES ('{sourceId}', 'Test Source')"
    do! execSql shared $"INSERT INTO public.test_emails (id, source_id, sender) VALUES ('{emailId}', '{sourceId}', 'test@test.com')"

    let! results =
        selectTask shared {
            for e in ``public``.test_emails do
            join s in ``public``.test_sources on (e.source_id = Some s.id)
            select (e.id, s.name)
        }

    let resultList = results |> Seq.toList
    Assert.AreEqual(1, resultList.Length, "Expected 1 joined result")
    let (eid, sname) = resultList.[0]
    Assert.AreEqual(emailId, eid)
    Assert.AreEqual("Test Source", sname)

    shared.RollbackTransaction()
}

// ============================================================
// Bug 2: Compound && in on' with .Value access
// ============================================================

[<Test>]
let ``Bug2: leftJoin' on' with compound AND and .Value access``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let sourceId = Guid.NewGuid()
    let userId = Guid.NewGuid()
    let prefId = Guid.NewGuid()

    do! execSql shared $"INSERT INTO public.test_sources (id, name) VALUES ('{sourceId}', 'Test Source')"
    do! execSql shared $"INSERT INTO public.test_preferences (id, source_id, user_id, priority) VALUES ('{prefId}', '{sourceId}', '{userId}', 10)"

    let! results =
        selectTask shared {
            for s in ``public``.test_sources do
            leftJoin' p in ``public``.test_preferences
            on' (p.Value.source_id = s.id && p.Value.user_id = userId)
            select (s.id, p)
        }

    let resultList = results |> Seq.toList
    Assert.AreEqual(1, resultList.Length, "Expected 1 result")
    let (sid, pref) = resultList.[0]
    Assert.AreEqual(sourceId, sid)
    Assert.IsTrue(pref.IsSome, "Expected matched preference")
    Assert.AreEqual(10, pref.Value.priority)

    shared.RollbackTransaction()
}

[<Test>]
let ``Bug2: leftJoin' on' compound AND where second condition doesn't match``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let sourceId = Guid.NewGuid()
    let userId = Guid.NewGuid()
    let otherUserId = Guid.NewGuid()
    let prefId = Guid.NewGuid()

    do! execSql shared $"INSERT INTO public.test_sources (id, name) VALUES ('{sourceId}', 'Test Source')"
    do! execSql shared $"INSERT INTO public.test_preferences (id, source_id, user_id, priority) VALUES ('{prefId}', '{sourceId}', '{otherUserId}', 5)"

    // Join with userId that doesn't match the preference's user_id
    let! results =
        selectTask shared {
            for s in ``public``.test_sources do
            leftJoin' p in ``public``.test_preferences
            on' (p.Value.source_id = s.id && p.Value.user_id = userId)
            select (s.id, p)
        }

    let resultList = results |> Seq.toList
    Assert.AreEqual(1, resultList.Length, "Expected 1 result (source exists)")
    let (sid, pref) = resultList.[0]
    Assert.AreEqual(sourceId, sid)
    Assert.IsTrue(pref.IsNone, "Expected no matched preference for different user")

    shared.RollbackTransaction()
}

[<Test>]
let ``Bug2: leftJoin' on' with compound AND on non-option columns``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let emailId = Guid.NewGuid()
    let sourceId = Guid.NewGuid()
    let articleId = Guid.NewGuid()

    do! execSql shared $"INSERT INTO public.test_sources (id, name) VALUES ('{sourceId}', 'Source')"
    do! execSql shared $"INSERT INTO public.test_emails (id, source_id, sender) VALUES ('{emailId}', '{sourceId}', 'test@test.com')"
    do! execSql shared $"INSERT INTO public.test_articles (id, email_id, title) VALUES ('{articleId}', '{emailId}', 'specific')"

    let! results =
        selectTask shared {
            for e in ``public``.test_emails do
            leftJoin' a in ``public``.test_articles
            on' (a.Value.email_id = e.id && a.Value.title = "specific")
            select (e.id, a)
        }

    let resultList = results |> Seq.toList
    Assert.AreEqual(1, resultList.Length, "Expected 1 result")
    let (eid, article) = resultList.[0]
    Assert.AreEqual(emailId, eid)
    Assert.IsTrue(article.IsSome, "Expected matched article")

    shared.RollbackTransaction()
}

// NOTE: Reversed form `(Some s.id = e.source_id)` is not valid in CE join syntax
// because s (inner) is not available in the outer key selector position.
// The outer key must reference the outer source and the inner key must reference the inner source.

[<Test>]
let ``Bug1: join with Some and additional WHERE conditions``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let sourceId = Guid.NewGuid()
    let emailId = Guid.NewGuid()
    let verifiedAt = DateTime.UtcNow.ToString("o")

    do! execSql shared $"INSERT INTO public.test_sources (id, name, verified_at) VALUES ('{sourceId}', 'Verified Source', '{verifiedAt}')"
    do! execSql shared $"INSERT INTO public.test_emails (id, source_id, sender, status) VALUES ('{emailId}', '{sourceId}', 'test@test.com', 'stored')"

    let! results =
        selectTask shared {
            for e in ``public``.test_emails do
            join s in ``public``.test_sources on (e.source_id = Some s.id)
            where (e.status = "stored" && s.verified_at <> None)
            select e
        }

    let resultList = results |> Seq.toList
    Assert.AreEqual(1, resultList.Length, "Expected 1 result with WHERE conditions")

    shared.RollbackTransaction()
}

// ============================================================
// Regression tests: patterns that work and must keep working
// ============================================================

[<Test>]
let ``Regression: <> None on Option column (IS NOT NULL)``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let sourceId = Guid.NewGuid()
    let emailId1 = Guid.NewGuid()
    let emailId2 = Guid.NewGuid()
    let verifiedAt = DateTime.UtcNow.ToString("o")

    do! execSql shared $"INSERT INTO public.test_sources (id, name) VALUES ('{sourceId}', 'Source')"
    do! execSql shared $"INSERT INTO public.test_emails (id, source_id, sender, verified_at) VALUES ('{emailId1}', '{sourceId}', 'a@test.com', '{verifiedAt}')"
    do! execSql shared $"INSERT INTO public.test_emails (id, sender) VALUES ('{emailId2}', 'b@test.com')"

    let! withVerified =
        selectTask shared {
            for e in ``public``.test_emails do
            where (e.verified_at <> None)
            select e.id
        }
    Assert.IsTrue(withVerified |> Seq.contains emailId1, "Should find email with verified_at")
    Assert.IsFalse(withVerified |> Seq.contains emailId2, "Should not find email without verified_at")

    shared.RollbackTransaction()
}

[<Test>]
let ``Regression: = None on Option column (IS NULL)``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let sourceId = Guid.NewGuid()
    let emailId1 = Guid.NewGuid()
    let emailId2 = Guid.NewGuid()
    let verifiedAt = DateTime.UtcNow.ToString("o")

    do! execSql shared $"INSERT INTO public.test_sources (id, name) VALUES ('{sourceId}', 'Source')"
    do! execSql shared $"INSERT INTO public.test_emails (id, source_id, sender, verified_at) VALUES ('{emailId1}', '{sourceId}', 'a@test.com', '{verifiedAt}')"
    do! execSql shared $"INSERT INTO public.test_emails (id, sender) VALUES ('{emailId2}', 'b@test.com')"

    let! withoutVerified =
        selectTask shared {
            for e in ``public``.test_emails do
            where (e.verified_at = None)
            select e.id
        }
    Assert.IsTrue(withoutVerified |> Seq.contains emailId2, "Should find email without verified_at")
    Assert.IsFalse(withoutVerified |> Seq.contains emailId1, "Should not find email with verified_at")

    shared.RollbackTransaction()
}

[<Test>]
let ``Regression: = None on leftJoin' binding (anti-join)``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let sourceId = Guid.NewGuid()
    let emailId1 = Guid.NewGuid()
    let emailId2 = Guid.NewGuid()
    let articleId = Guid.NewGuid()

    do! execSql shared $"INSERT INTO public.test_sources (id, name) VALUES ('{sourceId}', 'Source')"
    do! execSql shared $"INSERT INTO public.test_emails (id, source_id, sender) VALUES ('{emailId1}', '{sourceId}', 'a@test.com')"
    do! execSql shared $"INSERT INTO public.test_emails (id, source_id, sender) VALUES ('{emailId2}', '{sourceId}', 'b@test.com')"
    do! execSql shared $"INSERT INTO public.test_articles (id, email_id, title) VALUES ('{articleId}', '{emailId1}', 'Article')"

    // Anti-join: emails without articles
    let! emailsWithoutArticles =
        selectTask shared {
            for e in ``public``.test_emails do
            leftJoin' a in ``public``.test_articles
            on' (a.Value.email_id = e.id)
            where (a = None)
            select e.id
        }

    Assert.IsTrue(emailsWithoutArticles |> Seq.contains emailId2, "Email without article should be in anti-join result")
    Assert.IsFalse(emailsWithoutArticles |> Seq.contains emailId1, "Email with article should NOT be in anti-join result")

    shared.RollbackTransaction()
}

[<Test>]
let ``Regression: single-condition on' with .Value``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let sourceId = Guid.NewGuid()
    let prefId = Guid.NewGuid()
    let userId = Guid.NewGuid()

    do! execSql shared $"INSERT INTO public.test_sources (id, name) VALUES ('{sourceId}', 'Source')"
    do! execSql shared $"INSERT INTO public.test_preferences (id, source_id, user_id) VALUES ('{prefId}', '{sourceId}', '{userId}')"

    let! results =
        selectTask shared {
            for s in ``public``.test_sources do
            leftJoin' p in ``public``.test_preferences
            on' (p.Value.source_id = s.id)
            select (s.id, p)
        }

    let resultList = results |> Seq.toList
    Assert.AreEqual(1, resultList.Length)
    Assert.IsTrue((snd resultList.[0]).IsSome, "Expected matched preference")

    shared.RollbackTransaction()
}

[<Test>]
let ``Regression: =% ILIKE operator``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let emailId = Guid.NewGuid()

    do! execSql shared $"INSERT INTO public.test_emails (id, sender) VALUES ('{emailId}', 'alice@example.com')"

    let! results =
        selectTask shared {
            for e in ``public``.test_emails do
            where (e.sender =% "%alice%")
            select e.id
        }

    Assert.IsTrue(results |> Seq.contains emailId, "ILIKE should match 'alice'")

    shared.RollbackTransaction()
}

[<Test>]
let ``Regression: onConflictDoNothingWhereRaw with returning``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let userId = Guid.NewGuid()
    let eventId = Guid.NewGuid()

    let event1 = {
        ``public``.test_events.id = eventId
        ``public``.test_events.email_event_id = Some "evt-123"
        ``public``.test_events.user_id = userId
        ``public``.test_events.event_type = "open"
        ``public``.test_events.created_at = DateTime.UtcNow
    }

    // First insert should succeed and return the id
    let! (returnedId: Guid) =
        insertTask shared {
            for e in ``public``.test_events do
            entity event1
            onConflict e.email_event_id
            whereRawConflict "email_event_id IS NOT NULL"
            doNothing
            returning e.id
        }

    Assert.AreEqual(eventId, returnedId, "First insert should return the id")

    // Second insert with same email_event_id should be ignored
    let event2 = { event1 with id = Guid.NewGuid() }
    let! (returnedId2: Guid) =
        insertTask shared {
            for e in ``public``.test_events do
            entity event2
            onConflict e.email_event_id
            whereRawConflict "email_event_id IS NOT NULL"
            doNothing
            returning e.id
        }

    Assert.AreEqual(Guid.Empty, returnedId2, "Conflicting insert should return default (empty) guid")

    shared.RollbackTransaction()
}
