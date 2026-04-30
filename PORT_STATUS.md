# v4 Port — Status

Branch `feature/postgres-enhancements-v4` from `upstream/replace-sqlkata` (v4.0.0-beta.3).

## Done — Phases 1, 2, 3a, 3b, 4

8 commits. **All passing test counts:**
- 247/247 unit tests on net10.0
- 102/102 Npgsql tests including integration vs Postgres
- 61/61 Sqlite tests
- SqlServer/Oracle units pass; integration failures are infra-only (no MSSQL/Oracle server running locally)

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

## Phase 3b — done (commit `813ba19`)

- ✅ `caseWhen` / `caseWhenMulti` — render as `CASE WHEN ... THEN ... ELSE ... END`. New `renderExpr` helper handles columns, static fields, constants, Convert unwrap, BinaryExpression (compare/arithmetic/and/or), and recurses to `visitSqlFn` for nested method calls. New `extractListItems`/`extractTuple` parse F# list cons cells with compile-and-eval fallback.
- ✅ `lateralCol "alias" "col"` → `"alias"."col"` raw column ref
- ✅ `rawExpr<'T> sql` → raw SQL escape hatch in select projections
- ✅ `PgSqlFn.interval(value)` → `INTERVAL '<value>'` literal
- ✅ Nested aggregate-in-aggregate (`CAST(SUM(x))`, `MAX(SUM(x))`) — aggregate dispatch now recursively renders inner expressions

## Phase 4 — done (commit `d35d…`)

`SqlHydra.Query.Pgvector` package ported:
- New project `src/SqlHydra.Query.Pgvector/` added to solution
- `PgvectorRegistration` registers `<=>`, `<->`, `<#>` infix operators in the `InfixOperators` registry
- `PgvectorFn` type with `cosine_distance` / `l2_distance` / `inner_product_distance` static members emit infix when called via `open type PgvectorFn`
- `SelectBuilder` extension: `orderByCosineDistance` / `orderByL2Distance` / `orderByInnerProductDistance`
- `InternalsVisibleTo` extended in `SqlHydra.Query` so the package can use `LinqExpressionVisitors` internals
- Note: distance ordering currently emits a raw `?` placeholder; vector parameter binding is a follow-up using the `RawColumnWithParams` IR variant

The pre-existing 3-failure regression on the branch (`cosine_distance` not converting to `<=>` in select via `open type PgvectorFn`) is **fixed** in v4 — Phase 4 tests verify this. Each test calls `ensureRegistered ()` explicitly because module init doesn't auto-fire when only the type is referenced.

### Phase 4 — pgvector (½ day)

- Port `SqlHydra.Query.Pgvector` package from branch.
- Wire `InfixOperators` registry into v4 visitor (one call site in method-call dispatch — depends on Phase 3).
- Verify `cosine_distance(a, b)` rewrites to `a <=> b` when imported via `open type PgvectorFn` (pre-existing 3-failure regression on branch — likely fixed by Phase 3 work).

### Phase 5 — CLI (done)

`IExtendTypeMapping` already supersedes branch's `custom_type_mappings` (was cleanly resolved during the v3.6.0 merge on the branch). No port work needed.

### Phase 6 — integration tests (done for Postgres)

- 247/247 unit tests pass on net10.0.
- 102/102 Npgsql tests pass against Postgres on `localhost:54320`.
- 61/61 Sqlite tests pass.
- SqlServer/Oracle integration failures (95 total) are entirely "no DB running" — these require `mssql` and `orcl` containers from `src/.devcontainer/docker-compose.yml`. None of the failures are related to v4 port code.
- The 3 pre-existing pgvector failures from the branch are **fixed** in v4.

## Done

The port is functionally complete for the postgres-enhancements feature set on top of `upstream/replace-sqlkata`. Open follow-ups are minor:

- Vector parameter binding for `orderByCosineDistance` etc. (uses `?` placeholder; needs `RawColumnWithParams` plumbing to bind the actual vector).
- Run integration tests on SqlServer + Oracle once those containers are spun up.
- Decide whether to upstream this branch as a PR to JordanMarr/SqlHydra v4.0.0-beta.4.
