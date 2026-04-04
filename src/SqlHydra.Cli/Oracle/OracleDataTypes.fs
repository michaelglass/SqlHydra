module SqlHydra.Oracle.OracleDataTypes

open System.Data
open SqlHydra.Domain

/// A list of supported column type mappings
let supportedTypeMappings =
    [   // https://docs.oracle.com/cd/B19306_01/win.102/b14306/appendixa.htm
        "PLS_INTEGER",                                  "int",                          DbType.Int32
        "LONG",                                         "int64",                        DbType.Int64
        "NUMBER",                                       "decimal",                      DbType.Decimal
        "FLOAT",                                        "double",                       DbType.Double
        "BINARY_DOUBLE",                                "double",                       DbType.Double
        "BINARY_FLOAT",                                 "System.Single",                DbType.Single
        "REAL",                                         "System.Single",                DbType.Single
        "ROWID",                                        "string",                       DbType.String
        "UROWID",                                       "string",                       DbType.String
        "VARCHAR",                                      "string",                       DbType.String
        "VARCHAR2",                                     "string",                       DbType.String
        "NVARCHAR",                                     "string",                       DbType.String
        "NVARCHAR2",                                    "string",                       DbType.String
        "CHAR",                                         "string",                       DbType.String
        "XMLType",                                      "string",                       DbType.String
        "NCHAR",                                        "string",                       DbType.StringFixedLength
        "TEXT",                                         "string",                       DbType.String
        "NTEXT",                                        "string",                       DbType.String
        "CLOB",                                         "string",                       DbType.String
        "NCLOB",                                        "string",                       DbType.String
        "DATE",                                         "System.DateTime",              DbType.Date
        "TIMESTAMP",                                    "System.DateTime",              DbType.Date
        "TIMESTAMP WITH LOCAL TIME ZONE",               "System.DateTime",              DbType.Date
        "TIMESTAMP WITH TIME ZONE",                     "System.DateTime",              DbType.Date
        "INTERVAL DAY TO SECOND",                       "System.TimeSpan",              DbType.Time

        for x in 0 .. 9 do
            $"TIMESTAMP({x})",                          "System.DateTime",              DbType.Date
            $"TIMESTAMP({x}) WITH LOCAL TIME ZONE",     "System.DateTime",              DbType.Date
            $"TIMESTAMP({x}) WITH TIME ZONE",           "System.DateTime",              DbType.Date
            for y in 0 .. 9 do
                $"INTERVAL DAY({x}) TO SECOND({y})",    "System.TimeSpan",              DbType.Time

        "BFILE",                                        "byte[]",                       DbType.Binary
        "BLOB",                                         "byte[]",                       DbType.Binary
        "LONG RAW",                                     "byte[]",                       DbType.Binary
        "RAW",                                          "byte[]",                       DbType.Binary
    ]

let typeMappingsByName =
    supportedTypeMappings
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

/// Adjusts a NUMBER type mapping based on Oracle precision/scale metadata.
let adjustForPrecisionScale (mapping: TypeMapping) (precisionMaybe: int option) (scaleMaybe: int option) =
    match mapping.ColumnTypeAlias with
    | "NUMBER" ->
        match precisionMaybe, scaleMaybe with
        // Untyped NUMBER → double
        | None, None
        | Some 0, None
        | Some 0, Some -127 ->
            { mapping with ClrType = "double"; DbType = DbType.Double }

        // Fractional → decimal
        | _, Some s when s > 0 ->
            { mapping with ClrType = "decimal"; DbType = DbType.Decimal }

        // Integer NUMBER
        | Some p, Some 0 when p <= 9 ->
            { mapping with ClrType = "int"; DbType = DbType.Int32 }

        | Some p, Some 0 when p <= 18 ->
            { mapping with ClrType = "int64"; DbType = DbType.Int64 }

        // Large integer → decimal
        | Some _, Some 0 ->
            { mapping with ClrType = "decimal"; DbType = DbType.Decimal }

        // Fallback
        | _ ->
            { mapping with ClrType = "decimal"; DbType = DbType.Decimal }

    | _ ->
        mapping

let tryFindTypeMapping (ctx: TypeMappingContext) =
    typeMappingsByName.TryFind (ctx.Column.ProviderTypeName.ToUpper())
    |> Option.map (fun m -> adjustForPrecisionScale m ctx.Column.Precision ctx.Column.Scale)
