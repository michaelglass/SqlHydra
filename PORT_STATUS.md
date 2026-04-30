# v4 Port — Status

Branch `feature/postgres-enhancements-v4` from `upstream/replace-sqlkata` (v4.0.0-beta.3).

## Done — Phase 1 & 2

3 commits, **230/230 unit tests passing** (215 baseline + 15 new Phase 2 tests).

### Phase 1 — IR foundation (commit `5abce07`)

`QueryIR.fs` extended additively:
- `NullsOrdering` type and `OrderByColumnNulls` variant
- `JoinKind.LeftJoinLateral`; `JoinClause.Subquery: SelectQueryIR option`
- `SelectQueryIR.WithCtes`, `DistinctOn`, `Returning` (DELETE carrier)
- `WhereClause.Exists` / `NotExists`
- `SelectColumn.RawColumnWithParams`
- `SetClause` type
- New `InsertType` cases: `OnConflictDoUpdateCoalesce`, `OnConflictDoNothingWhereRaw`, `OnConflictDoNothingRawTarget`
- `InsertQueryIR.FromSelect`, `Returning`
- `UpdateQueryIR.SetRaws`, `Returning`
- `DeleteQueryIR.Returning`

`SqlEmitterBase.fs` renders all new IR fields. Postgres + SQLite emitters override `EmitReturning` and the new conflict variants.

### Phase 2 — builder ops (commits `6ce1ef9`, `875dea1`)

| Op | Builder | Test | Notes |
|---|---|---|---|
| `whereExists` / `whereNotExists` | Select | ✅ | Subquery → `WhereClause.Exists/NotExists` |
| `havingRaw` | Select | ✅ | (overloaded with/without params) |
| `orderByRaw` | Select | ✅ | |
| `orderByAlias` / `orderByAliasDesc` | Select | ✅ | Quoted identifier |
| `nullsLast` / `nullsFirst` | Select | ✅ | Updates last `OrderBy` clause |
| `setRaw` (col, fragment, params) | Update | ✅ | (overloaded for no-params) |
| `returning` | Insert + Update + Delete | ✅ | Postgres RETURNING; threads through specs |
| `fromSelect` | Insert | ✅ | INSERT INTO (cols) SELECT … |
| `distinctOn` | Select (Npgsql ext) | ✅ | DISTINCT ON (col) prefix |
| `lateralJoin` | Select (Npgsql ext) | ✅ | LEFT JOIN LATERAL (subquery) AS alias |
| `cte<'T>` / `cteFrom<'T>` | Table | ✅ | WITH alias AS (subquery) prefix |
| `onConflictDoUpdateCoalesce` | Insert (Npgsql ext) | ✅ | COALESCE(EXCLUDED.col, col) |
| `onConflictDoNothingWhereRaw` | Insert (Npgsql ext) | wired, no test yet | Partial-index conflict |
| `onConflictDoNothingRawTarget` | Insert (Npgsql ext) | ✅ | Expression-index conflict |

`Core.fs` extended: `InsertQuerySpec` gained `Returning`/`FromSelect`; `UpdateQuerySpec` gained `RawSetValues`/`Returning`. `fromInsert`/`fromUpdate` thread these through. `InsertAsyncBuilder.Run` now treats `FromSelect.IsSome` as non-empty.

## Remaining

### Phase 2 leftovers — needs expression-visitor work

These were planned for Phase 2 but require touching `LinqExpressionVisitors.fs`, so deferred to Phase 3:

- `caseWhen` / `caseWhenMulti` (SELECT projection — produces `RawColumnWithParams`)
- `castAs<'T>` (CAST in SELECT)
- `countDistinct` aggregate
- `lateralCol` (referencing lateral subquery columns by string alias)
- `rawExpr` (escape hatch for raw column expressions)
- `PgSqlFn.interval` (binds to a parameter typed as PG `interval`)

### Phase 3 — visitor patches (~2-4 days)

Re-apply each branch fix against v4's `LinqExpressionVisitors.fs` (887 lines, heavily refactored from branch's version):

| Branch commit | Fix |
|---|---|
| `e1c35f6` | BlockExpression hoisting in visitSelect (multi-join projections) |
| `daf9a7f` | Anonymous record select Block+Assign pattern (.NET 10) |
| `60b180b` | BlockExpression scalar hoisting in select (castAs/lateralCol) |
| `d13a4c4` | visitAlias hardening for .NET 10 |
| `bb31cd4` | castAs<'T> method dispatch → CAST(x AS sqlType) |
| `f206cb0` | nested aggregate-in-aggregate / aggregate-in-function |
| `40423fb` | aggregates and arithmetic inside caseWhen |
| `0135905` | BlockExpression in HAVING / ORDER BY / infix precedence |
| `35d44d3` | BlockExpression in visitOrderByPropertySelector (multi-join) |
| `9b53913` | BlockExpression / get_Item in visitPropertiesSelector (groupBy after leftJoin') |
| `0f35a0f` | tuple destructuring in correlate predicates |
| `9d5059b` | captured bool expressions in conditional `&&`/`\|\|` |
| `eb75931` | auto-parameterize static fields (Guid.Empty, DateTime.MinValue) |
| `f00316c` | aggregate arithmetic and infix in SELECT |
| `b6d5e83` | infix operator registration applied in anonymous record select (pgvector) |

Most of these are surgical, but the new visitor's structure means each must be re-applied by reading branch commit + writing fresh code, not by patch.

### Phase 4 — pgvector (½ day)

- Port `SqlHydra.Query.Pgvector` package from branch.
- Wire `InfixOperators` registry into v4 visitor (one call site in method-call dispatch — depends on Phase 3).
- Verify `cosine_distance(a, b)` rewrites to `a <=> b` when imported via `open type PgvectorFn` (pre-existing 3-failure regression on branch — likely fixed by Phase 3 work).

### Phase 5 — CLI (done)

`IExtendTypeMapping` already supersedes branch's `custom_type_mappings` (was cleanly resolved during the v3.6.0 merge on the branch). No port work needed.

### Phase 6 — integration tests (1-2 days)

- All v4 unit tests pass (230/230). Integration tests need Postgres on :54320.
- Run integration tests after Phase 3 to surface .NET 10 regressions that need visitor patches.
- Investigate the 3 pre-existing pgvector test failures from the branch (`cosine_distance` not converting to `<=>` in select projections via `open type PgvectorFn`) — likely closed by Phase 3.

## Estimated remaining: 4-7 engineer-days

Phase 3 dominates the remaining work. The IR + builder foundation is now in place, so each visitor patch is a focused, testable change.
