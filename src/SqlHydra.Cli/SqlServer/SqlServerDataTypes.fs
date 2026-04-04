module SqlHydra.SqlServer.SqlServerDataTypes

open System.Data
open SqlHydra.Domain
open Microsoft.SqlServer.Types

// https://docs.microsoft.com/en-us/dotnet/framework/data/adonet/sql-server-data-type-mappings
/// A list of supported column type mappings
let supportedTypeMappings isLegacy =
    [// ColumnTypeAlias         ClrType                                     DbType                      ProviderDbType
        "UNIQUEIDENTIFIER",     "System.Guid",                              DbType.Guid,                Some (nameof SqlDbType.UniqueIdentifier)
        "BIT",                  "bool",                                     DbType.Boolean,             Some (nameof SqlDbType.Bit)
        "INT",                  "int",                                      DbType.Int32,               Some (nameof SqlDbType.Int)
        "BIGINT",               "int64",                                    DbType.Int64,               Some (nameof SqlDbType.BigInt)
        "SMALLINT",             "int16",                                    DbType.Int16,               Some (nameof SqlDbType.SmallInt)
        "TINYINT",              "byte",                                     DbType.Byte,                Some (nameof SqlDbType.TinyInt)
        "FLOAT",                "double",                                   DbType.Double,              Some (nameof SqlDbType.Float)
        "REAL",                 "System.Single",                            DbType.Single,              Some (nameof SqlDbType.Real)
        "DECIMAL",              "decimal",                                  DbType.Decimal,             Some (nameof SqlDbType.Decimal)
        "NUMERIC",              "decimal",                                  DbType.Decimal,             Some (nameof SqlDbType.Decimal)
        "MONEY",                "decimal",                                  DbType.Decimal,             Some (nameof SqlDbType.Money)
        "SMALLMONEY",           "decimal",                                  DbType.Decimal,             Some (nameof SqlDbType.SmallMoney)
        "VARCHAR",              "string",                                   DbType.String,              Some (nameof SqlDbType.VarChar)
        "NVARCHAR",             "string",                                   DbType.String,              Some (nameof SqlDbType.NVarChar)
        "CHAR",                 "string",                                   DbType.String,              Some (nameof SqlDbType.Char)
        "NCHAR",                "string",                                   DbType.StringFixedLength,   Some (nameof SqlDbType.NChar)
        "TEXT",                 "string",                                   DbType.String,              Some (nameof SqlDbType.Text)
        "NTEXT",                "string",                                   DbType.String,              Some (nameof SqlDbType.NText)
        "DATETIMEOFFSET",       "System.DateTimeOffset",                    DbType.DateTimeOffset,      Some (nameof SqlDbType.DateTimeOffset)

        if isLegacy then
         "DATE",                "System.DateTime",                          DbType.Date,                Some (nameof SqlDbType.Date)
         "TIME",                "System.TimeSpan",                          DbType.Time,                Some (nameof SqlDbType.Time)
        else
         "DATE",                "System.DateOnly",                          DbType.Date,                Some (nameof SqlDbType.Date)
         "TIME",                "System.TimeOnly",                          DbType.Time,                Some (nameof SqlDbType.Time)

        "DATETIME",             "System.DateTime",                          DbType.DateTime,            Some (nameof SqlDbType.DateTime)
        "DATETIME2",            "System.DateTime",                          DbType.DateTime2,           Some (nameof SqlDbType.DateTime2)
        "SMALLDATETIME",        "System.DateTime",                          DbType.DateTime,            Some (nameof SqlDbType.SmallDateTime)
        "VARBINARY",            "byte[]",                                   DbType.Binary,              Some (nameof SqlDbType.VarBinary)
        "BINARY",               "byte[]",                                   DbType.Binary,              Some (nameof SqlDbType.Binary)
        "IMAGE",                "byte[]",                                   DbType.Binary,              Some (nameof SqlDbType.Image)
        "ROWVERSION",           "byte[]",                                   DbType.Binary,              Some (nameof SqlDbType.Binary)
        "TIMESTAMP",            "byte[]",                                   DbType.Binary,              Some (nameof SqlDbType.Binary)
        "SQL_VARIANT",          "obj",                                      DbType.Object,              Some (nameof SqlDbType.Variant)
        "HIERARCHYID",          "Microsoft.SqlServer.Types.SqlHierarchyId", DbType.Object,              Some (nameof SqlHierarchyId)

        // UNSUPPORTED COLUMN TYPES
        //"XML",                "System.Data.SqlTypes.SqlXml",              DbType.Xml,                 None
        //"GEOGRAPHY",          "Microsoft.SqlServer.Types.SqlGeography",   DbType.Object,              None
        //"GEOMETRY",           "Microsoft.SqlServer.Types.SqlGeometry",    DbType.Object,              None

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
        map.TryFind (ctx.Column.ProviderTypeName.ToUpper())
