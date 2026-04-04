module SqlHydra.Sqlite.SqliteDataTypes

open System.Data
open SqlHydra.Domain

/// A list of supported column type mappings
let supportedTypeMappings isLegacy =
    [
        "smallint",         "int16",            DbType.Int16
        "int",              "int",              DbType.Int32
        "real",             "double",           DbType.Double
        "single",           "System.Single",    DbType.Single
        "float",            "double",           DbType.Double
        "double",           "double",           DbType.Double
        "money",            "decimal",          DbType.Decimal
        "currency",         "decimal",          DbType.Decimal
        "decimal",          "decimal",          DbType.Decimal
        "numeric",          "decimal",          DbType.Decimal
        "bit",              "bool",             DbType.Boolean
        "yesno",            "bool",             DbType.Boolean
        "logical",          "bool",             DbType.Boolean
        "bool",             "bool",             DbType.Boolean
        "boolean",          "bool",             DbType.Boolean
        "tinyint",          "byte",             DbType.Byte
        "integer",          "int64",            DbType.Int64
        "identity",         "int64",            DbType.Int64
        "integer identity", "int64",            DbType.Int64
        "counter",          "int64",            DbType.Int64
        "autoincrement",    "int64",            DbType.Int64
        "long",             "int64",            DbType.Int64
        "bigint",           "int64",            DbType.Int64
        "binary",           "byte[]",           DbType.Binary
        "varbinary",        "byte[]",           DbType.Binary
        "blob",             "byte[]",           DbType.Binary
        "image",            "byte[]",           DbType.Binary
        "general",          "byte[]",           DbType.Binary
        "oleobject",        "byte[]",           DbType.Binary
        "varchar",          "string",           DbType.String
        "nvarchar",         "string",           DbType.String
        "memo",             "string",           DbType.String
        "longtext",         "string",           DbType.String
        "longvarchar",      "string",           DbType.String
        "note",             "string",           DbType.String
        "text",             "string",           DbType.String
        "ntext",            "string",           DbType.String
        "string",           "string",           DbType.String
        "char",             "string",           DbType.String
        "nchar",            "string",           DbType.String
        "xml",              "string",           DbType.Xml
        "datetime",         "System.DateTime",  DbType.DateTime
        "smalldate",        "System.DateTime",  DbType.DateTime
        "timestamp",        "System.DateTime",  DbType.DateTime

        if isLegacy then
         "date",            "System.DateTime",  DbType.DateTime
         "time",            "System.DateTime",  DbType.DateTime
        else
         "date",            "System.DateOnly",  DbType.DateTime
         "time",            "System.TimeOnly",  DbType.DateTime

        "uniqueidentifier", "System.Guid",      DbType.Guid
        "guid",             "System.Guid",      DbType.Guid
    ]

let typeMappingsByName isLegacy =
    supportedTypeMappings isLegacy
    |> List.map (fun (columnTypeAlias, clrType, dbType) ->
        columnTypeAlias,
        {
            TypeMapping.ColumnTypeAlias = columnTypeAlias
            TypeMapping.ClrType = clrType
            TypeMapping.DbType = dbType
            TypeMapping.ProviderDbType = None
        }
    )
    |> Map.ofList

let tryFindTypeMapping isLegacy =
    let map = typeMappingsByName isLegacy
    fun (ctx: TypeMappingContext) ->
        map.TryFind (ctx.Column.ProviderTypeName.ToLower().Trim())
