namespace Sqlite.CustomTypes

open SqlHydra.Domain

/// A custom type alias for string.
type Text = string

/// Maps "text" and "string" SQLite column types to the custom Text type.
type TextTypeMapping() =
    interface IExtendTypeMapping with
        member _.Extend(baseTryFind) =
            fun (ctx: TypeMappingContext) ->
                match ctx.Column.ProviderTypeName.Trim().ToLower() with
                | "text"
                | "string" ->
                    Some {
                        TypeMapping.ColumnTypeAlias = ctx.Column.ProviderTypeName
                        TypeMapping.ClrType = "Sqlite.CustomTypes.Text"
                        TypeMapping.DbType = System.Data.DbType.String
                        TypeMapping.ProviderDbType = None
                    }
                | _ ->
                    baseTryFind ctx
