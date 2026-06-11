module SqlHydra.SqlServer.Provider

open SqlHydra.Domain

let instance: ISqlHydraDbProvider =
    { new ISqlHydraDbProvider with
        member _.Id = "mssql"
        member _.Name = "SqlHydra.SqlServer"
        member _.Type = SqlServer
        member _.DefaultReaderType = "Microsoft.Data.SqlClient.SqlDataReader"
        member _.DefaultProvider = "Microsoft.Data.SqlClient"
        member _.SqlEmitter = "SqlHydra.Query.SqlServerEmitter()"
        member _.ProviderConnectionType = "Microsoft.Data.SqlClient.SqlConnection"
        member _.GetSchema(cfg, isLegacy, extensions) = SqlServerSchemaProvider.getSchema(cfg, isLegacy, extensions)
        member _.RenderEnumRegistration(_, _) = None
    }
