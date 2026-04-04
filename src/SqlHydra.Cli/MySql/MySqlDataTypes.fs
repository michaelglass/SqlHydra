module SqlHydra.MySql.MySqlDataTypes

open System.Data
open MySql.Data.MySqlClient
open SqlHydra.Domain

/// A list of supported column type mappings
let supportedTypeMappings isLegacy = // https://dev.mysql.com/doc/refman/9.0/en/data-types.html
    [// ColumnTypeAlias                 ClrType                 DbType                  ProviderDbType
        "bit",                          "int16",                DbType.Int16,           Some (nameof MySqlDbType.Bit)
        "tinyint",                      "int16",                DbType.Int16,           Some (nameof MySqlDbType.Int16)
        "bool",                         "int16",                DbType.Boolean,         Some (nameof MySqlDbType.Int16)
        "boolean",                      "int16",                DbType.Boolean,         Some (nameof MySqlDbType.Int16)
        "smallint",                     "int16",                DbType.Int16,           Some (nameof MySqlDbType.Int16)
        "mediumint",                    "int",                  DbType.Int32,           Some (nameof MySqlDbType.Int24)
        "int",                          "int",                  DbType.Int32,           Some (nameof MySqlDbType.Int32)
        "integer",                      "int",                  DbType.Int32,           Some (nameof MySqlDbType.Int32)
        "bigint",                       "int64",                DbType.Int64,           Some (nameof MySqlDbType.Int64)
        "decimal",                      "decimal",              DbType.Decimal,         Some (nameof MySqlDbType.Decimal)
        "dec",                          "decimal",              DbType.Decimal,         Some (nameof MySqlDbType.Decimal)
        "float",                        "float",                DbType.Single,          Some (nameof MySqlDbType.Float)
        "double",                       "double",               DbType.Double,          Some (nameof MySqlDbType.Double)
        "double precision",             "double",               DbType.Double,          Some (nameof MySqlDbType.Double)
        "char",                         "string",               DbType.String,          Some (nameof MySqlDbType.String)
        "varchar",                      "string",               DbType.String,          Some (nameof MySqlDbType.VarChar)
        "nvarchar",                     "string",               DbType.String,          Some (nameof MySqlDbType.VarChar)
        "binary",                       "byte[]",               DbType.Binary,          Some (nameof MySqlDbType.Binary)
        "char BYTE",                    "byte[]",               DbType.Binary,          Some (nameof MySqlDbType.Binary)
        "varbinary",                    "byte[]",               DbType.Binary,          Some (nameof MySqlDbType.VarBinary)
        "tinyblob",                     "byte[]",               DbType.Binary,          Some (nameof MySqlDbType.TinyBlob)
        "blob",                         "byte[]",               DbType.Binary,          Some (nameof MySqlDbType.Blob)
        "mediumblob",                   "byte[]",               DbType.Binary,          Some (nameof MySqlDbType.MediumBlob)
        "longblob",                     "byte[]",               DbType.Binary,          Some (nameof MySqlDbType.LongBlob)
        "tinytext",                     "string",               DbType.String,          Some (nameof MySqlDbType.TinyText)
        "text",                         "string",               DbType.String,          Some (nameof MySqlDbType.Text)
        "mediumtext",                   "string",               DbType.String,          Some (nameof MySqlDbType.MediumText)
        "longtext",                     "string",               DbType.String,          Some (nameof MySqlDbType.LongText)
        "enum",                         "string",               DbType.String,          Some (nameof MySqlDbType.Enum)
        "set",                          "string",               DbType.String,          Some (nameof MySqlDbType.Set)
        "json",                         "string",               DbType.String,          Some (nameof MySqlDbType.JSON)

        if isLegacy then
         "date",                        "System.DateTime",      DbType.DateTime,        Some (nameof MySqlDbType.Date)
         "time",                        "System.DateTime",      DbType.DateTime,        Some (nameof MySqlDbType.Time)
        else
         "date",                        "System.DateOnly",      DbType.DateTime,        Some (nameof MySqlDbType.Date)
         "time",                        "System.TimeOnly",      DbType.Time,            Some (nameof MySqlDbType.Time)

        "datetime",                     "System.DateTime",      DbType.DateTime,        Some (nameof MySqlDbType.DateTime)
        "timestamp",                    "System.DateTime",      DbType.DateTime,        Some (nameof MySqlDbType.Timestamp)
        "year",                         "int16",                DbType.Int16,           Some (nameof MySqlDbType.Year)
        // skipped unsupported
        "bool",                         "bool",                 DbType.Boolean,         Some (nameof MySqlDbType.Int16)
        "boolean",                      "bool",                 DbType.Boolean,         Some (nameof MySqlDbType.Int16)
        "float4",                       "float",                DbType.Single,          Some (nameof MySqlDbType.Float)
        "float8",                       "double",               DbType.Double,          Some (nameof MySqlDbType.Double)
        "numeric",                      "decimal",              DbType.Decimal,         Some (nameof MySqlDbType.Decimal)
        "long",                         "string",               DbType.String,          Some (nameof MySqlDbType.MediumText)
    ]

let typeMappingsByName isLegacy =
    supportedTypeMappings isLegacy
    |> List.map (fun (columnTypeAlias, clrType, dbType, providerDbType) ->
        columnTypeAlias,
        {
            TypeMapping.ColumnTypeAlias = columnTypeAlias
            TypeMapping.ClrType = clrType
            TypeMapping.DbType = dbType
            TypeMapping.ProviderDbType = providerDbType
        }
    )
    |> Map.ofList

let tryFindTypeMapping isLegacy =
    let map = typeMappingsByName isLegacy
    fun (ctx: TypeMappingContext) ->
        map.TryFind (ctx.Column.ProviderTypeName.ToLower().Trim())
