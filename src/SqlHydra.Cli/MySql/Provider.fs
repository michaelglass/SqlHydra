module SqlHydra.MySql.Provider

open SqlHydra.Domain

let instance: ISqlHydraDbProvider =
    { new ISqlHydraDbProvider with
        member _.Id = "mysql"
        member _.Name = "SqlHydra.MySql"
        member _.Type = MySql
        member _.DefaultReaderType = "System.Data.Common.DbDataReader"
        member _.DefaultProvider = "MySql.Data"
        member _.SqlKataCompiler = "SqlKata.Compilers.MySqlCompiler()"
        member _.ProviderConnectionType = "MySql.Data.MySqlClient.MySqlConnection"
        member _.GetSchema(cfg, isLegacy, extensions) = MySqlSchemaProvider.getSchema(cfg, isLegacy, extensions)
    }
