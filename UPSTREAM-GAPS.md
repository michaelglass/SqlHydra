# Feedback for SqlHydra

## 1. General Gaps

### ~~Anonymous record + aggregate column aliasing bug~~ (FIXED)

Fixed in `daf9a7f`. .NET 10 F# compiles anonymous records as Block+Assign expressions
instead of Invoke(Lambda) chains. `visitSelect` now handles both patterns.

### Silent runtime failures after SqlHydra version upgrade

When upgrading SqlHydra.Query to a new version, everything compiles but queries fail at runtime until you regenerate types with the CLI. The generated `HydraReader` and table declarations silently become incompatible with the new Query library. This is disorienting — you get runtime exceptions deep inside query execution with no indication that the fix is "re-run the CLI tool."

It would be helpful to have some kind of version contract between `SqlHydra.Query` and the generated types — e.g., a version stamp or interface marker in the generated code that the Query library checks at compile time (or at least at startup with a clear error message like "Generated types were produced by SqlHydra.Cli vX but SqlHydra.Query vY expects vZ — please regenerate").

**Workaround**: Remember to always re-run `dotnet sqlhydra` after upgrading the NuGet package. No way to enforce this automatically.

### IExtendNaming is disabled

The `IExtendNaming` interface exists but is commented out in `Console.fs`. Noted as a TODO — just flagging that we'd use it when it's ready.

**Workaround**: Manual renaming in post-processing or in the consuming code.

## 2. Extensibility Gaps

These are things we ran into while building a PostgreSQL extension (pgvector support, custom types, lateral joins, etc.) on top of the v3.6.0 extensibility system.

### No query-side extension point

`IExtendTypeMapping` covers CLI code generation (mapping database types to F# types), but there's no equivalent for the Query side. Building a database extension like pgvector requires two things:

1. **CLI**: Map `vector` column type to `Pgvector.Vector` — covered by `IExtendTypeMapping`
2. **Query**: Emit `<=>` infix operator when the user writes `cosine_distance(a, b)` — no extension point exists

We built an `InfixOperators` registry (a `ConcurrentDictionary` that maps function names to SQL operators) as a workaround, but it requires explicit initialization (`ensureRegistered()`) and lives outside any formal extension contract.

Similarly, features like `DISTINCT ON` and `NULLS FIRST/LAST` require post-compilation SQL transforms. We implemented these as `ConditionalWeakTable`-based side stores (`DistinctOnStore`, `NullsStore`) that patch the compiled SQL string after SqlKata produces it. This works but is fragile.

**Suggestion**: An `IExtendQuery` interface (or similar) that allows extensions to register:
- Custom infix operators (function name -> SQL operator)
- Custom post-compilation SQL transforms

**Workaround**: The `InfixOperators` module and `ConditionalWeakTable` side stores work, but require manual initialization and aren't discoverable through the extension system.

### No extension auto-discovery at query time

`Extensions.scanProject` / `Extensions.loadNamed` only runs during CLI code generation. At query time, there's no equivalent — pgvector operators must be registered via explicit `ensureRegistered()` calls. F# module initialization in separate assemblies is unreliable (the compiler can defer it), so `[<AutoOpen>]` modules with `do` bindings don't reliably trigger.

**Suggestion**: Assembly-level attributes like `[<assembly: SqlHydraQueryExtension(typeof<MyExtension>)>]` that get scanned at `QueryContext` initialization, or a convention-based discovery mechanism.

**Workaround**: Call `ensureRegistered()` explicitly at application startup. Works, but easy to forget.
