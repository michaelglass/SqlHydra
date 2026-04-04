module SqlHydra.Npgsql.Provider

open SqlHydra.Domain

let instance: ISqlHydraDbProvider =
    { new ISqlHydraDbProvider with
        member _.Id = "npgsql"
        member _.Name = "SqlHydra.Npgsql"
        member _.Type = Npgsql
        member _.DefaultReaderType = "Npgsql.NpgsqlDataReader"
        member _.DefaultProvider = "Npgsql"
        member _.SqlKataCompiler = "SqlKata.Compilers.PostgresCompiler()"
        member _.ProviderConnectionType = "Npgsql.NpgsqlConnection"
        member _.GetSchema(cfg, isLegacy, extensions) = NpgsqlSchemaProvider.getSchema(cfg, isLegacy, extensions)
    }
