module SqlHydra.Npgsql.Provider

open SqlHydra.Domain

let private backticks = Fantomas.FCS.Syntax.PrettyNaming.NormalizeIdentifierBackticks

/// Renders the additive Npgsql enum-registration helper module.
/// Returns Some fragment when there is at least one enum, else None.
let renderEnumRegistration (_cfg: Config) (db: Schema) : string option =
    if db.Enums |> List.isEmpty then
        None
    else
        let enums =
            db.Enums
            |> List.sortBy (fun e -> e.Schema, e.Name)

        let fragment = stringBuffer {
            "[<RequireQualifiedAccess>]"
            "module Enums ="
            indent {
                "/// Registers all generated PostgreSQL enum types with the data source builder."
                "let register (builder: Npgsql.NpgsqlDataSourceBuilder) : Npgsql.NpgsqlDataSourceBuilder ="
                indent {
                    for enum in enums do
                        let typeArg = $"{backticks enum.Schema}.{backticks enum.Name}"
                        let pgName =
                            if enum.Schema = "public" then enum.Name
                            else $"{enum.Schema}.{enum.Name}"
                        $"builder.MapEnum<{typeArg}>(\"{pgName}\") |> ignore"
                    "builder"
                }
            }
        }

        Some fragment

let instance: ISqlHydraDbProvider =
    { new ISqlHydraDbProvider with
        member _.Id = "npgsql"
        member _.Name = "SqlHydra.Npgsql"
        member _.Type = Npgsql
        member _.DefaultReaderType = "Npgsql.NpgsqlDataReader"
        member _.DefaultProvider = "Npgsql"
        member _.SqlEmitter = "SqlHydra.Query.PostgresEmitter()"
        member _.ProviderConnectionType = "Npgsql.NpgsqlConnection"
        member _.GetSchema(cfg, isLegacy, extensions) = NpgsqlSchemaProvider.getSchema(cfg, isLegacy, extensions)
        member _.RenderEnumRegistration(cfg, db) = renderEnumRegistration cfg db
    }
