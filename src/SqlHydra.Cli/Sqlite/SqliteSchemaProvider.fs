module SqlHydra.Sqlite.SqliteSchemaProvider

open System.Data
open System.Data.SQLite
open SqlHydra.Domain
open SqlHydra

let dbNullOpt<'T> (o: obj) : 'T option =
    match o with
    | :? System.DBNull -> None
    | _ -> o :?> 'T |> Some

let getSchema (cfg: Config, isLegacy: bool, extensions: IExtendTypeMapping list) : Schema =
    use conn = new SQLiteConnection(cfg.ConnectionString)
    conn.Open()
    let sTables = conn.GetSchema("Tables", cfg.Filters.TryGetRestrictionsByKey("Tables"))
    let sColumns = conn.GetSchema("Columns", cfg.Filters.TryGetRestrictionsByKey("Columns"))

    // SQLite only supports one schema per file.
    // We will override to be main; otherwise, all columns will have "sqlite_default_schema"
    let defaultSchema = "main"

    let allColumns =
        sColumns.Rows
        |> Seq.cast<DataRow>
        |> Seq.map (fun col ->
            {
                ColumnSchema.Catalog = col.["TABLE_CATALOG"] :?> string
                Schema = defaultSchema // col.["TABLE_SCHEMA"] :?> string
                Table = col.["TABLE_NAME"] :?> string
                Name = col.["COLUMN_NAME"] :?> string
                ProviderTypeName = col.["DATA_TYPE"] :?> string
                Ordinal = col.["ORDINAL_POSITION"] :?> int
                IsNullable = col.["IS_NULLABLE"] :?> bool
                IsPrimaryKey = col.["PRIMARY_KEY"] :?> bool
                Precision = None
                Scale = None
                IsComputed = false
                DefaultValue = None
            }
        )
        |> Seq.sortBy (fun column -> column.Ordinal)

    let columnsByTable =
        allColumns
        |> Seq.groupBy (fun col -> col.Catalog, col.Schema, col.Table)
        |> Map.ofSeq

    let tryFindTypeMapping =
        let baseTryFind = SqliteDataTypes.tryFindTypeMapping isLegacy
        extensions |> List.fold (fun acc (ext: IExtendTypeMapping) -> ext.Extend(acc)) baseTryFind

    let tableSchemas =
        sTables.Rows
        |> Seq.cast<DataRow>
        |> Seq.map (fun tbl ->
            {|
                Catalog = tbl.["TABLE_CATALOG"] :?> string
                Schema = tbl.["TABLE_SCHEMA"] |> dbNullOpt<string> |> Option.defaultValue defaultSchema
                Name  = tbl.["TABLE_NAME"] :?> string
                Type = tbl.["TABLE_TYPE"] :?> string
            |}
        )
        |> Seq.filter (fun tbl -> tbl.Type <> "SYSTEM_TABLE")
        |> Seq.map (fun tbl ->
            let cols =
                columnsByTable
                |> Map.tryFind (tbl.Catalog, tbl.Schema, tbl.Name)
                |> Option.map Seq.toList
                |> Option.defaultValue []
            {
                TableSchema.Catalog = tbl.Catalog
                Schema = tbl.Schema
                Name = tbl.Name
                Type = if tbl.Type = "table" then TableType.Table else TableType.View
                Columns = cols
            }
        )
        |> Seq.toList
        |> SchemaFilters.filterTables cfg.Filters

    let tables =
        tableSchemas
        |> Seq.choose (fun tableSchema ->
            let supportedColumns =
                tableSchema.Columns
                |> List.choose (fun col ->
                    let ctx = { TypeMappingContext.Table = tableSchema; TypeMappingContext.Column = col }
                    tryFindTypeMapping ctx
                    |> Option.map (fun typeMapping ->
                        {
                            Column.Name = col.Name
                            Column.IsNullable = col.IsNullable
                            Column.TypeMapping = typeMapping
                            Column.IsPK = col.IsPrimaryKey
                        }
                    )
                )

            let filteredColumns =
                supportedColumns
                |> SchemaFilters.filterColumns cfg.Filters tableSchema.Schema tableSchema.Name
                |> Seq.toList

            if filteredColumns |> Seq.isEmpty then
                None
            else
                Some {
                    Table.Catalog = tableSchema.Catalog
                    Table.Schema = tableSchema.Schema
                    Table.Name = tableSchema.Name
                    Table.Type = tableSchema.Type
                    Table.Columns = filteredColumns
                    Table.TotalColumns = tableSchema.Columns |> List.length
                }
        )
        |> Seq.toList

    {
        Tables = tables
        Enums = []
    }
