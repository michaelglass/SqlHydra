module Npgsql.``Generated Type Regression Tests``

open System
open Swensen.Unquote
open SqlHydra.Query
open SqlHydra.Query.NpgsqlExtensions
open NUnit.Framework
open Npgsql.DB

// Use the SqlHydra-GENERATED types (with ProviderDbType attributes)
// These produce different expression trees than manually-defined CLIMutable records
open Npgsql.GeneratedTestTypes

let db =
    let dataSource = Npgsql.NpgsqlDataSourceBuilder(connectionString).Build()
    Npgsql.GeneratedTestTypes.QueryContextFactory.Create(dataSource, sqlLogger = printf "SQL: %O")

let private execSql (shared: QueryContext) (sql: string) =
    use cmd = shared.Connection.CreateCommand()
    cmd.Transaction <- shared.Transaction |> Option.defaultValue null
    cmd.CommandText <- sql
    cmd.ExecuteNonQuery() |> ignore

// ============================================================
// Bug 1: join on (optionalCol = Some nonNullCol) with generated types
// ============================================================

[<Test>]
let ``Generated: join on Option column = Some non-nullable PK``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let sourceId = Guid.NewGuid()
    let emailId = Guid.NewGuid()

    execSql shared $"INSERT INTO public.test_sources (id, name) VALUES ('{sourceId}', 'Gen Source')"
    execSql shared $"INSERT INTO public.test_emails (id, source_id, sender) VALUES ('{emailId}', '{sourceId}', 'gen@test.com')"

    let! results =
        selectTask shared {
            for e in ``public``.test_emails do
            join s in ``public``.test_sources on (e.source_id = Some s.id)
            select (e.id, s.name)
        }

    let resultList = results |> Seq.toList
    Assert.AreEqual(1, resultList.Length, "Expected 1 joined result with generated types")
    let (eid, sname) = resultList.[0]
    Assert.AreEqual(emailId, eid)
    Assert.AreEqual("Gen Source", sname)

    shared.RollbackTransaction()
}

[<Test>]
let ``Generated: join on Option column with additional WHERE conditions``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let sourceId = Guid.NewGuid()
    let emailId = Guid.NewGuid()
    let verifiedAt = DateTime.UtcNow.ToString("o")

    execSql shared $"INSERT INTO public.test_sources (id, name, verified_at) VALUES ('{sourceId}', 'Verified Gen', '{verifiedAt}')"
    execSql shared $"INSERT INTO public.test_emails (id, source_id, sender, status) VALUES ('{emailId}', '{sourceId}', 'gen@test.com', 'stored')"

    let! results =
        selectTask shared {
            for e in ``public``.test_emails do
            join s in ``public``.test_sources on (e.source_id = Some s.id)
            where (e.status = "stored" && s.verified_at <> None)
            select e
        }

    let resultList = results |> Seq.toList
    Assert.AreEqual(1, resultList.Length, "Expected 1 result with WHERE conditions on generated types")

    shared.RollbackTransaction()
}

// ============================================================
// Bug 2: Compound && in on' with .Value access on generated types
// ============================================================

[<Test>]
let ``Generated: leftJoin' on' with compound AND and .Value access``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let sourceId = Guid.NewGuid()
    let userId = Guid.NewGuid()
    let prefId = Guid.NewGuid()

    execSql shared $"INSERT INTO public.test_sources (id, name) VALUES ('{sourceId}', 'Gen Source')"
    execSql shared $"INSERT INTO public.test_preferences (id, source_id, user_id, priority) VALUES ('{prefId}', '{sourceId}', '{userId}', 10)"

    let! results =
        selectTask shared {
            for s in ``public``.test_sources do
            leftJoin' p in ``public``.test_preferences
            on' (p.Value.source_id = s.id && p.Value.user_id = userId)
            select (s.id, p)
        }

    let resultList = results |> Seq.toList
    Assert.AreEqual(1, resultList.Length, "Expected 1 result with generated types")
    let (sid, pref) = resultList.[0]
    Assert.AreEqual(sourceId, sid)
    Assert.IsTrue(pref.IsSome, "Expected matched preference with generated types")
    Assert.AreEqual(10, pref.Value.priority)

    shared.RollbackTransaction()
}

[<Test>]
let ``Generated: leftJoin' on' compound AND where second condition doesn't match``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let sourceId = Guid.NewGuid()
    let userId = Guid.NewGuid()
    let otherUserId = Guid.NewGuid()
    let prefId = Guid.NewGuid()

    execSql shared $"INSERT INTO public.test_sources (id, name) VALUES ('{sourceId}', 'Gen Source')"
    execSql shared $"INSERT INTO public.test_preferences (id, source_id, user_id, priority) VALUES ('{prefId}', '{sourceId}', '{otherUserId}', 5)"

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
    Assert.IsTrue(pref.IsNone, "Expected no matched preference for different user with generated types")

    shared.RollbackTransaction()
}

// ============================================================
// Bug 3: setRaw with generated types
// ============================================================

[<Test>]
let ``Generated: setRaw combined with set on generated type``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let sourceId = Guid.NewGuid()

    execSql shared $"INSERT INTO public.test_sources (id, name, priority) VALUES ('{sourceId}', 'Original', 5)"

    let! result =
        updateTask shared {
            for s in ``public``.test_sources do
            set s.name "Updated"
            setRaw s.priority "COALESCE(?, priority)" [| box 10 |]
            where (s.id = sourceId)
        }

    Assert.AreEqual(1, result, "Expected 1 row updated")

    // Verify both updates worked
    let! updated =
        selectTask shared {
            for s in ``public``.test_sources do
            where (s.id = sourceId)
            select (s.name, s.priority)
            tryHead
        }

    Assert.IsTrue(updated.IsSome, "Expected to find updated row")
    let (name, priority) = updated.Value
    Assert.AreEqual("Updated", name)
    Assert.AreEqual(10, priority)

    shared.RollbackTransaction()
}

[<Test>]
let ``Generated: set on generated type column updates correctly``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let sourceId = Guid.NewGuid()

    execSql shared $"INSERT INTO public.test_sources (id, name, priority) VALUES ('{sourceId}', 'Original', 5)"

    let! result =
        updateTask shared {
            for s in ``public``.test_sources do
            set s.name "Updated"
            set s.priority 42
            where (s.id = sourceId)
        }

    Assert.AreEqual(1, result, "Expected 1 row updated")

    let! updated =
        selectTask shared {
            for s in ``public``.test_sources do
            where (s.id = sourceId)
            select (s.name, s.priority)
            tryHead
        }

    Assert.IsTrue(updated.IsSome, "Expected to find updated row")
    let (name, priority) = updated.Value
    Assert.AreEqual("Updated", name)
    Assert.AreEqual(42, priority)

    shared.RollbackTransaction()
}

// Bug 3b: setRaw-only (no `set`) — the __SETRAW_PLACEHOLDER__ must be replaced, not appended
[<Test>]
let ``Generated: setRaw-only without set replaces column correctly``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let sourceId = Guid.NewGuid()

    execSql shared $"INSERT INTO public.test_sources (id, name, priority) VALUES ('{sourceId}', 'Original', 5)"

    // setRaw with no `set` — was broken: placeholder got parameterized, duplicate SET column appended
    let! result =
        updateTask shared {
            for s in ``public``.test_sources do
            setRaw s.priority "COALESCE(?, priority)" [| box 99 |]
            where (s.id = sourceId)
        }

    Assert.AreEqual(1, result, "Expected 1 row updated with setRaw-only")

    let! updated =
        selectTask shared {
            for s in ``public``.test_sources do
            where (s.id = sourceId)
            select s.priority
            tryHead
        }

    Assert.IsTrue(updated.IsSome, "Expected to find updated row")
    Assert.AreEqual(99, updated.Value, "setRaw-only: priority should be updated via raw expression")

    shared.RollbackTransaction()
}

// Bug 3c: setRaw string concatenation — the exact pattern from still-failing.md
// (column || separator || value, using raw SQL on generated types)
[<Test>]
let ``Generated: setRaw string concatenation appends to existing column value``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let sourceId = Guid.NewGuid()

    execSql shared $"INSERT INTO public.test_sources (id, name, priority) VALUES ('{sourceId}', 'Hello', 5)"

    // Mirrors the still-failing.md pattern: raw_notes || separator || @notes
    let! result =
        updateTask shared {
            for s in ``public``.test_sources do
            setRaw s.name "name || ?" [| box " World" |]
            where (s.id = sourceId)
        }

    Assert.AreEqual(1, result, "Expected 1 row updated with string concat setRaw")

    let! updated =
        selectTask shared {
            for s in ``public``.test_sources do
            where (s.id = sourceId)
            select s.name
            tryHead
        }

    Assert.IsTrue(updated.IsSome, "Expected to find updated row")
    Assert.AreEqual("Hello World", updated.Value, "setRaw string concat should append to existing value")

    shared.RollbackTransaction()
}

// ============================================================
// Migration 1 fix: multi-join where outer key comes from accumulated tuple
// e.g., join A on (e.fk = Some a.id); join B on (a.id = b.fk)
// The second join's outer key selector has type (E * A) -> Key
// F# compiles tuple destructuring as BlockExpression — visitJoin must unwrap it
// ============================================================

[<Test>]
let ``Generated: multi-join second join outer key from accumulated tuple``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let sourceId = Guid.NewGuid()
    let emailId = Guid.NewGuid()
    let userId = Guid.NewGuid()
    let prefId = Guid.NewGuid()

    execSql shared $"INSERT INTO public.test_sources (id, name) VALUES ('{sourceId}', 'Source')"
    execSql shared $"INSERT INTO public.test_emails (id, source_id, sender) VALUES ('{emailId}', '{sourceId}', 'a@test.com')"
    execSql shared $"INSERT INTO public.test_preferences (id, source_id, user_id, priority) VALUES ('{prefId}', '{sourceId}', '{userId}', 7)"

    // join 1: email.source_id (Option<Guid>) = Some source.id (Guid)
    // join 2: source.id = pref.source_id  ← outer key comes from (email * source) tuple
    let! results =
        selectTask shared {
            for e in ``public``.test_emails do
            join s in ``public``.test_sources on (e.source_id = Some s.id)
            join p in ``public``.test_preferences on (s.id = p.source_id)
            select (e.id, s.name, p.priority)
        }

    let resultList = results |> Seq.toList
    Assert.AreEqual(1, resultList.Length, "Expected 1 row from multi-join")
    let (eid, sname, pri) = resultList.[0]
    Assert.AreEqual(emailId, eid)
    Assert.AreEqual("Source", sname)
    Assert.AreEqual(7, pri)

    shared.RollbackTransaction()
}

[<Test>]
let ``Generated: multi-join with Some on first join and plain on second join``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let sourceId = Guid.NewGuid()
    let emailId = Guid.NewGuid()
    let articleId = Guid.NewGuid()

    execSql shared $"INSERT INTO public.test_sources (id, name) VALUES ('{sourceId}', 'Src')"
    execSql shared $"INSERT INTO public.test_emails (id, source_id, sender) VALUES ('{emailId}', '{sourceId}', 'b@test.com')"
    execSql shared $"INSERT INTO public.test_articles (id, email_id, title) VALUES ('{articleId}', '{emailId}', 'Art')"

    // join 1: email.source_id = Some source.id
    // join 2: email.id = article.email_id  ← outer key is e.id from (email * source) tuple
    let! results =
        selectTask shared {
            for e in ``public``.test_emails do
            join s in ``public``.test_sources on (e.source_id = Some s.id)
            join a in ``public``.test_articles on (e.id = a.email_id)
            select (e.id, s.name, a.title)
        }

    let resultList = results |> Seq.toList
    Assert.AreEqual(1, resultList.Length, "Expected 1 row")
    let (eid, sname, title) = resultList.[0]
    Assert.AreEqual(emailId, eid)
    Assert.AreEqual("Src", sname)
    Assert.AreEqual("Art", title)

    shared.RollbackTransaction()
}

// ============================================================
// Migration 3 fix: where clause on outer table after leftJoin'
// e.g., leftJoin' p in prefs; on' (...); where (outerTable.col = x)
// F# compiles the tuple-destructuring lambda as BlockExpression — visitWhere must unwrap it
// ============================================================

[<Test>]
let ``Generated: where on outer table column after leftJoin'``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let source1Id = Guid.NewGuid()
    let source2Id = Guid.NewGuid()
    let userId = Guid.NewGuid()
    let prefId = Guid.NewGuid()

    execSql shared $"INSERT INTO public.test_sources (id, name, priority) VALUES ('{source1Id}', 'Alpha', 5)"
    execSql shared $"INSERT INTO public.test_sources (id, name, priority) VALUES ('{source2Id}', 'Beta', 3)"
    execSql shared $"INSERT INTO public.test_preferences (id, source_id, user_id, priority) VALUES ('{prefId}', '{source1Id}', '{userId}', 10)"

    let! results =
        selectTask shared {
            for s in ``public``.test_sources do
            leftJoin' p in ``public``.test_preferences
            on' (p.Value.source_id = s.id && p.Value.user_id = userId)
            where (s.name = "Alpha")
            select (s.id, p)
        }

    let resultList = results |> Seq.toList
    Assert.AreEqual(1, resultList.Length, "WHERE on outer table should filter to just Alpha")
    let (sid, pref) = resultList.[0]
    Assert.AreEqual(source1Id, sid)
    Assert.IsTrue(pref.IsSome)
    Assert.AreEqual(10, pref.Value.priority)

    shared.RollbackTransaction()
}

[<Test>]
let ``Generated: where on outer table column after leftJoin' — unmatched row returns None``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let sourceId = Guid.NewGuid()
    let userId = Guid.NewGuid()

    execSql shared $"INSERT INTO public.test_sources (id, name, priority) VALUES ('{sourceId}', 'OnlySource', 5)"
    // No preference inserted — left join returns None for p

    let! results =
        selectTask shared {
            for s in ``public``.test_sources do
            leftJoin' p in ``public``.test_preferences
            on' (p.Value.source_id = s.id && p.Value.user_id = userId)
            where (s.id = sourceId)
            select (s.id, p)
        }

    let resultList = results |> Seq.toList
    Assert.AreEqual(1, resultList.Length, "Source should appear even with no preference (LEFT JOIN)")
    let (_, pref) = resultList.[0]
    Assert.IsTrue(pref.IsNone, "No preference inserted, so p should be None")

    shared.RollbackTransaction()
}

// ============================================================
// Chained join scalability: do BlockExpression fixes scale to 3+ joins?
// ============================================================

[<Test>]
let ``Generated: 3-way join chain — third join outer key from 2-element tuple``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let sourceId = Guid.NewGuid()
    let emailId  = Guid.NewGuid()
    let userId   = Guid.NewGuid()
    let prefId   = Guid.NewGuid()
    let artId    = Guid.NewGuid()

    execSql shared $"INSERT INTO public.test_sources (id, name) VALUES ('{sourceId}', 'Src3')"
    execSql shared $"INSERT INTO public.test_emails (id, source_id, sender) VALUES ('{emailId}', '{sourceId}', 'c@test.com')"
    execSql shared $"INSERT INTO public.test_preferences (id, source_id, user_id, priority) VALUES ('{prefId}', '{sourceId}', '{userId}', 9)"
    execSql shared $"INSERT INTO public.test_articles (id, email_id, title) VALUES ('{artId}', '{emailId}', 'Title3')"

    // join 1: email.source_id (Option<Guid>) = Some source.id
    // join 2: source.id = pref.source_id          ← outer from (email * source)
    // join 3: email.id  = article.email_id         ← outer key e.id from (email * source * pref)
    let! results =
        selectTask shared {
            for e in ``public``.test_emails do
            join s in ``public``.test_sources on (e.source_id = Some s.id)
            join p in ``public``.test_preferences on (s.id = p.source_id)
            join a in ``public``.test_articles on (e.id = a.email_id)
            select (e.sender, s.name, p.priority, a.title)
        }

    let resultList = results |> Seq.toList
    Assert.AreEqual(1, resultList.Length, "Expected 1 row from 3-way join chain")
    let (sender, sname, pri, title) = resultList.[0]
    Assert.AreEqual("c@test.com", sender)
    Assert.AreEqual("Src3",  sname)
    Assert.AreEqual(9, pri)
    Assert.AreEqual("Title3", title)

    shared.RollbackTransaction()
}

[<Test>]
let ``Generated: 3-way join with where on column from second joined table``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let source1Id = Guid.NewGuid()
    let source2Id = Guid.NewGuid()
    let email1Id  = Guid.NewGuid()
    let email2Id  = Guid.NewGuid()
    let userId    = Guid.NewGuid()
    let pref1Id   = Guid.NewGuid()
    let pref2Id   = Guid.NewGuid()

    execSql shared $"INSERT INTO public.test_sources (id, name, priority) VALUES ('{source1Id}', 'SrcA', 5)"
    execSql shared $"INSERT INTO public.test_sources (id, name, priority) VALUES ('{source2Id}', 'SrcB', 3)"
    execSql shared $"INSERT INTO public.test_emails (id, source_id, sender) VALUES ('{email1Id}', '{source1Id}', 'd@test.com')"
    execSql shared $"INSERT INTO public.test_emails (id, source_id, sender) VALUES ('{email2Id}', '{source2Id}', 'e@test.com')"
    execSql shared $"INSERT INTO public.test_preferences (id, source_id, user_id, priority) VALUES ('{pref1Id}', '{source1Id}', '{userId}', 1)"
    execSql shared $"INSERT INTO public.test_preferences (id, source_id, user_id, priority) VALUES ('{pref2Id}', '{source2Id}', '{userId}', 2)"

    // 3-way join with a where clause on s.name (column from first joined table, in 3-element tuple state)
    let! results =
        selectTask shared {
            for e in ``public``.test_emails do
            join s in ``public``.test_sources on (e.source_id = Some s.id)
            join p in ``public``.test_preferences on (s.id = p.source_id)
            where (s.name = "SrcA")
            select (e.sender, s.name, p.priority)
        }

    let resultList = results |> Seq.toList
    Assert.AreEqual(1, resultList.Length, "WHERE on 2nd-joined table column should filter correctly")
    let (sender, sname, pri) = resultList.[0]
    Assert.AreEqual("d@test.com", sender)
    Assert.AreEqual("SrcA", sname)
    Assert.AreEqual(1, pri)

    shared.RollbackTransaction()
}

// ============================================================
// leftJoin' scalability: multiple leftJoin's, mixed join + leftJoin'
// ============================================================

[<Test>]
let ``Generated: two leftJoin's chained — both sides populated``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let sourceId = Guid.NewGuid()
    let emailId  = Guid.NewGuid()
    let userId   = Guid.NewGuid()
    let prefId   = Guid.NewGuid()
    let artId    = Guid.NewGuid()

    execSql shared $"INSERT INTO public.test_sources (id, name) VALUES ('{sourceId}', 'SrcLJ')"
    execSql shared $"INSERT INTO public.test_emails (id, source_id, sender) VALUES ('{emailId}', '{sourceId}', 'lj@test.com')"
    execSql shared $"INSERT INTO public.test_preferences (id, source_id, user_id, priority) VALUES ('{prefId}', '{sourceId}', '{userId}', 42)"
    execSql shared $"INSERT INTO public.test_articles (id, email_id, title) VALUES ('{artId}', '{emailId}', 'LJArt')"

    let! results =
        selectTask shared {
            for e in ``public``.test_emails do
            leftJoin' s in ``public``.test_sources
            on' (s.Value.id = e.source_id.Value)
            leftJoin' a in ``public``.test_articles
            on' (a.Value.email_id = e.id)
            select (e.id, s, a)
        }

    let resultList = results |> Seq.toList
    Assert.AreEqual(1, resultList.Length)
    let (eid, sOpt, aOpt) = resultList.[0]
    Assert.AreEqual(emailId, eid)
    Assert.IsTrue(sOpt.IsSome)
    Assert.AreEqual("SrcLJ", sOpt.Value.name)
    Assert.IsTrue(aOpt.IsSome)
    Assert.AreEqual("LJArt", aOpt.Value.title)

    shared.RollbackTransaction()
}

[<Test>]
let ``Generated: two leftJoin's — second side unmatched returns None``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let sourceId = Guid.NewGuid()
    let emailId  = Guid.NewGuid()

    execSql shared $"INSERT INTO public.test_sources (id, name) VALUES ('{sourceId}', 'SrcLJ2')"
    execSql shared $"INSERT INTO public.test_emails (id, source_id, sender) VALUES ('{emailId}', '{sourceId}', 'lj2@test.com')"
    // No articles inserted — second leftJoin' should produce None

    let! results =
        selectTask shared {
            for e in ``public``.test_emails do
            leftJoin' s in ``public``.test_sources
            on' (s.Value.id = e.source_id.Value)
            leftJoin' a in ``public``.test_articles
            on' (a.Value.email_id = e.id)
            select (e.id, s, a)
        }

    let resultList = results |> Seq.toList
    Assert.AreEqual(1, resultList.Length)
    let (eid, sOpt, aOpt) = resultList.[0]
    Assert.AreEqual(emailId, eid)
    Assert.IsTrue(sOpt.IsSome)
    Assert.IsTrue(aOpt.IsNone, "No article inserted — should be None")

    shared.RollbackTransaction()
}

[<Test>]
let ``Generated: two leftJoin's with where on outer table``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let src1Id = Guid.NewGuid()
    let src2Id = Guid.NewGuid()
    let em1Id  = Guid.NewGuid()
    let em2Id  = Guid.NewGuid()

    execSql shared $"INSERT INTO public.test_sources (id, name) VALUES ('{src1Id}', 'Keep')"
    execSql shared $"INSERT INTO public.test_sources (id, name) VALUES ('{src2Id}', 'Drop')"
    execSql shared $"INSERT INTO public.test_emails (id, source_id, sender) VALUES ('{em1Id}', '{src1Id}', 'keep@test.com')"
    execSql shared $"INSERT INTO public.test_emails (id, source_id, sender) VALUES ('{em2Id}', '{src2Id}', 'drop@test.com')"

    let! results =
        selectTask shared {
            for e in ``public``.test_emails do
            leftJoin' s in ``public``.test_sources
            on' (s.Value.id = e.source_id.Value)
            leftJoin' a in ``public``.test_articles
            on' (a.Value.email_id = e.id)
            where (e.sender = "keep@test.com")
            select (e.sender, s, a)
        }

    let resultList = results |> Seq.toList
    Assert.AreEqual(1, resultList.Length, "WHERE on outer table should filter to 1 row")
    let (sender, _, _) = resultList.[0]
    Assert.AreEqual("keep@test.com", sender)

    shared.RollbackTransaction()
}

[<Test>]
let ``Generated: join then leftJoin' — mixed inner and outer join``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let sourceId = Guid.NewGuid()
    let emailId  = Guid.NewGuid()
    let userId   = Guid.NewGuid()
    let prefId   = Guid.NewGuid()

    execSql shared $"INSERT INTO public.test_sources (id, name) VALUES ('{sourceId}', 'MixSrc')"
    execSql shared $"INSERT INTO public.test_emails (id, source_id, sender) VALUES ('{emailId}', '{sourceId}', 'mix@test.com')"
    execSql shared $"INSERT INTO public.test_preferences (id, source_id, user_id, priority) VALUES ('{prefId}', '{sourceId}', '{userId}', 5)"
    // No article — the leftJoin' should return None for article

    let! results =
        selectTask shared {
            for e in ``public``.test_emails do
            join s in ``public``.test_sources on (e.source_id = Some s.id)
            leftJoin' a in ``public``.test_articles
            on' (a.Value.email_id = e.id)
            select (e.sender, s.name, a)
        }

    let resultList = results |> Seq.toList
    Assert.AreEqual(1, resultList.Length)
    let (sender, sname, aOpt) = resultList.[0]
    Assert.AreEqual("mix@test.com", sender)
    Assert.AreEqual("MixSrc", sname)
    Assert.IsTrue(aOpt.IsNone, "No article — leftJoin' should be None")

    shared.RollbackTransaction()
}

[<Test>]
let ``Generated: join then leftJoin' with where on inner-joined column``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let src1Id = Guid.NewGuid()
    let src2Id = Guid.NewGuid()
    let em1Id  = Guid.NewGuid()
    let em2Id  = Guid.NewGuid()
    let artId  = Guid.NewGuid()

    execSql shared $"INSERT INTO public.test_sources (id, name) VALUES ('{src1Id}', 'TargetSrc')"
    execSql shared $"INSERT INTO public.test_sources (id, name) VALUES ('{src2Id}', 'OtherSrc')"
    execSql shared $"INSERT INTO public.test_emails (id, source_id, sender) VALUES ('{em1Id}', '{src1Id}', 'target@test.com')"
    execSql shared $"INSERT INTO public.test_emails (id, source_id, sender) VALUES ('{em2Id}', '{src2Id}', 'other@test.com')"
    execSql shared $"INSERT INTO public.test_articles (id, email_id, title) VALUES ('{artId}', '{em1Id}', 'TgtArt')"

    let! results =
        selectTask shared {
            for e in ``public``.test_emails do
            join s in ``public``.test_sources on (e.source_id = Some s.id)
            leftJoin' a in ``public``.test_articles
            on' (a.Value.email_id = e.id)
            where (s.name = "TargetSrc")
            select (e.sender, s.name, a)
        }

    let resultList = results |> Seq.toList
    Assert.AreEqual(1, resultList.Length, "WHERE on inner-joined column should filter correctly")
    let (sender, sname, aOpt) = resultList.[0]
    Assert.AreEqual("target@test.com", sender)
    Assert.AreEqual("TargetSrc", sname)
    Assert.IsTrue(aOpt.IsSome)
    Assert.AreEqual("TgtArt", aOpt.Value.title)

    shared.RollbackTransaction()
}
