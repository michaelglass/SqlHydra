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
// Bug 3: setRaw with generated types (combined with set to avoid
// separate setRaw-only placeholder bug)
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
