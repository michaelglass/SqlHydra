module SqlHydra.Sqlite.Provider

open SqlHydra.Domain

let instance: ISqlHydraDbProvider =
    { new ISqlHydraDbProvider with
        member _.Id = "sqlite"
        member _.Name = "SqlHydra.Sqlite"
        member _.Type = Sqlite
        member _.DefaultReaderType = "System.Data.Common.DbDataReader"
        member _.DefaultProvider = "System.Data.SQLite"
        member _.SqlKataCompiler = "SqlKata.Compilers.SqliteCompiler()"
        member _.ProviderConnectionType = "Microsoft.Data.Sqlite.SqliteConnection"
        member _.GetSchema(cfg, isLegacy, extensions) = SqliteSchemaProvider.getSchema(cfg, isLegacy, extensions)
    }
