# v4 Port — Status

Branch `feature/postgres-enhancements-v4` from `upstream/replace-sqlkata` (v4.0.0-beta.3).

## Done — Phases 1, 2, 3a

5 commits, **236/236 unit tests passing** (215 baseline + 15 Phase 2 + 6 Phase 3a tests).

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

## Phase 3a — visitor patches done

Commit `562a505` + static-field patch:

- ✅ `countDistinct` aggregate (branch `3b6db2e`) — recognized in 3 visitor patterns; renders via new `renderAggregate` helper as `COUNT(DISTINCT col)` at all 5 dispatch sites.
- ✅ `castAs<'T>` (branch `bb31cd4`) — `sqlTypeForClrType` helper maps F# return type to SQL `FLOAT`/`INTEGER`/`BIGINT`/`NUMERIC`/`TEXT`/`BOOLEAN`; wired into `visitSqlFn`. Side-fix: `renderArg` now unwraps `UnaryExpression Convert` so numeric widening works.
- ✅ `InfixOperators` registry (branch `b6d5e83`) — module added to `QueryFunctions.fs`; visitor checks for 2-arg method calls registered as infix and emits `left OP right`. Foundation for pgvector wiring in Phase 4.
- ✅ Static-field auto-parameterization (branch `eb75931`) — `renderArg` now handles `Member mem` with null `Expression` (e.g. `Guid.Empty`, `DateTime.MinValue`, `String.Empty`) by evaluating via `Expression.Lambda(...).Compile().DynamicInvoke()` and rendering as inline SQL literal.

## Phase 3 — surprise: most BlockExpression patches no longer needed

When porting Phase 3 it became clear that **upstream/replace-sqlkata's `ExpressionNormalizer` already centralizes BlockExpression handling**. `Normalizer.VisitBlock` builds a `VariableInliner` substitution map for non-tuple-deconstruction variables and inlines them; `visitExpression` then unwraps Lambda/Block/Invoke noise centrally. This is conceptually the same fix as the branch's per-visitor patches but applied once, cleanly.

Patches **NOT needed** in v4 (handled by ExpressionNormalizer):

| Branch commit | Status | Why obsolete |
|---|---|---|
| `e1c35f6` | obsolete | BlockExpression hoisting in visitSelect — done by Normalizer |
| `60b180b` | obsolete | BlockExpression scalar hoisting in select — done by Normalizer |
| `d13a4c4` | obsolete | visitAlias hardening — Normalizer eliminates Block ahead of visitAlias |
| `0135905` | obsolete | BlockExpression in HAVING / ORDER BY — Normalizer |
| `35d44d3` | obsolete | BlockExpression in visitOrderByPropertySelector — Normalizer |
| `9b53913` | obsolete | BlockExpression / get_Item in visitPropertiesSelector — Normalizer |
| `0f35a0f` | obsolete | tuple destructuring in correlate — Normalizer preserves tuple deconstruction by design |

Verify by running v4's existing 215 unit tests — they exercise multi-join, leftJoin', groupBy-after-leftJoin', and other patterns that branch needed BlockExpression patches for. All pass on .NET 10. If a regression appears later, port the targeted patch then.

## Phase 3b — still TODO (can be done in a follow-up session)

| Branch commit | Description | Notes |
|---|---|---|
| `daf9a7f` | Anonymous record select Block+Assign | Verify v4's anon-record path handles .NET 10 — likely already covered by Normalizer |
| `f206cb0` | Nested aggregate-in-aggregate (`MAX(SUM(x))`) | Needs visitor support; consider `renderAggregate` extension |
| `40423fb` | Aggregates and arithmetic inside `caseWhen` | Depends on caseWhen port |
| `caseWhen` / `caseWhenMulti` | CASE WHEN expression | Needs visitor support for predicate compilation in select projection. Branch parameter-threads through `renderExpressionAsSql`; v4 would need similar threading or a `RawColumnWithParams` emit path |
| `9d5059b` | Captured bool in conditional `&&`/`\|\|` | Verify v4 handles; likely already in Normalizer |
| `f00316c` | Aggregate arithmetic + infix in SELECT | Verify; partly covered by `renderArg` Convert unwrapping |
| `lateralCol` | Reference lateral subquery columns by string | Small `RawColumn` helper |
| `rawExpr` | Escape hatch for raw select column | Small helper that emits `RawColumn` directly |
| `PgSqlFn.interval` | PG `interval` typed parameter | Postgres-only stub fn |

### Phase 4 — pgvector (½ day)

- Port `SqlHydra.Query.Pgvector` package from branch.
- Wire `InfixOperators` registry into v4 visitor (one call site in method-call dispatch — depends on Phase 3).
- Verify `cosine_distance(a, b)` rewrites to `a <=> b` when imported via `open type PgvectorFn` (pre-existing 3-failure regression on branch — likely fixed by Phase 3 work).

### Phase 5 — CLI (done)

`IExtendTypeMapping` already supersedes branch's `custom_type_mappings` (was cleanly resolved during the v3.6.0 merge on the branch). No port work needed.

### Phase 6 — integration tests (1-2 days)

- All v4 unit tests pass (236/236). Integration tests need Postgres on :54320.
- Run integration tests after Phase 3 to surface any .NET 10 regressions that need visitor patches.
- Investigate the 3 pre-existing pgvector test failures from the branch (`cosine_distance` not converting to `<=>` in select projections via `open type PgvectorFn`) — likely closed by Phase 3a's `InfixOperators` integration once Phase 4 wires up pgvector.

## Estimated remaining: 2-4 engineer-days

The big surprise from Phase 3a: most of the branch's `BlockExpression`-related patches turned out to be obsolete because `ExpressionNormalizer` already does the same job centrally. What remains is `caseWhen` (the largest single piece — needs parameter threading), pgvector wiring (mechanical), and integration test verification.
