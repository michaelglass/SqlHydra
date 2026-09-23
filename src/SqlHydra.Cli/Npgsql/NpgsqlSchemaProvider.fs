module SqlHydra.Npgsql.NpgsqlSchemaProvider

open System.Data
open SqlHydra.Domain
open SqlHydra

/// A column's generation flags, named as `pg_attribute` names them.
/// https://www.postgresql.org/docs/current/catalog-pg-attribute.html
type PgAttribute =
    { AttGenerated: string
      AttIdentity: string }

/// True for a column PostgreSQL writes itself: generated ('s' stored, 'v' virtual) or
/// identity ALWAYS ('a'). Identity BY DEFAULT ('d') is writable and must not match.
/// `AttGenerated` is compared, not matched, so a kind added in a later PostgreSQL is caught.
let isDatabaseGenerated (att: PgAttribute) =
    att.AttGenerated <> "" || att.AttIdentity = "a"

/// One column, named. A tuple key here would let the schema and table be probed in the
/// wrong order — which compiles, finds nothing, and marks nothing read-only.
type ColumnRef =
    { Schema: string
      Table: string
      Column: string }

/// True for an ordinary base table, false for a view.
///
/// Npgsql's `GetSchema("Tables")` reports TABLE_TYPE straight from
/// `information_schema.tables`, so a plain table arrives as the SQL-standard
/// "BASE TABLE" — never the literal "table" this used to test for, which meant every
/// PostgreSQL table was typed as a view. The `views` and `materialized views` rows
/// appended further down carry our own "view" / "materialized view" labels instead, so
/// testing by exclusion keeps this right regardless of how a future Npgsql spells the
/// base-table case.
let isBaseTableType tableType =
    tableType <> "view" && tableType <> "materialized view"

let getSchema (cfg: Config, isLegacy: bool, extensions: IExtendTypeMapping list) : Schema =
    use conn = new Npgsql.NpgsqlConnection(cfg.ConnectionString)
    conn.Open()
    // NOTE: GetSchema will fail if a Postgres enum doesn't exists in a custom schema but not in public schema.
    // Error: "type {enum name} does not exist"
    // This is a Postgres issue, not a SqlHydra issue.
    let sTables = conn.GetSchema("Tables", cfg.Filters.TryGetRestrictionsByKey("Tables"))
    let sViews = conn.GetSchema("Views", cfg.Filters.TryGetRestrictionsByKey("Views"))

    // MaterializedViews requires Npgsql v8 or greater (which requires net8 or greater).
#if NET8_0_OR_GREATER
    let sMaterializedViews = conn.GetSchema("MaterializedViews", cfg.Filters.TryGetRestrictionsByKey("MaterializedViews"))
#else
    let sMaterializedViews = new DataTable()
#endif

    let pks =
        let sql =
            """
            SELECT
                tc.table_schema,
                tc.constraint_name,
                tc.table_name,
                kcu.column_name,
                ccu.table_schema AS foreign_table_schema,
                ccu.table_name AS foreign_table_name,
                ccu.column_name AS foreign_column_name
            FROM
                information_schema.table_constraints AS tc
            JOIN information_schema.key_column_usage AS kcu
                ON tc.constraint_name = kcu.constraint_name
                AND tc.table_schema = kcu.table_schema
            JOIN information_schema.constraint_column_usage AS ccu
                ON ccu.constraint_name = tc.constraint_name
                AND ccu.table_schema = tc.table_schema
            WHERE tc.constraint_type = 'PRIMARY KEY';
            """

        use cmd = new Npgsql.NpgsqlCommand(sql, conn)
        use rdr = cmd.ExecuteReader()
        [
            while rdr.Read() do
                rdr.["TABLE_SCHEMA"] :?> string,
                rdr.["TABLE_NAME"] :?> string,
                rdr.["COLUMN_NAME"] :?> string
        ]
        |> Set.ofList

    let generatedColumns =
        let sql =
            """
            SELECT
                pg_namespace.nspname AS table_schema,
                pg_class.relname AS table_name,
                pg_attribute.attname AS column_name,
                pg_attribute.attgenerated::text AS attgenerated,
                pg_attribute.attidentity::text AS attidentity
            FROM pg_attribute
            INNER JOIN pg_class ON pg_class.oid = pg_attribute.attrelid
            INNER JOIN pg_namespace ON pg_namespace.oid = pg_class.relnamespace
            WHERE
                -- ordinary (r) and partitioned (p) tables; nothing else can be written
                pg_class.relkind in ('r', 'p') AND
                pg_attribute.attnum >= 1 AND
                NOT pg_attribute.attisdropped AND
                pg_namespace.nspname not in ('pg_catalog', 'information_schema');
            """

        use cmd = new Npgsql.NpgsqlCommand(sql, conn)
        use rdr = cmd.ExecuteReader()
        [
            while rdr.Read() do
                let att =
                    { AttGenerated = rdr["attgenerated"] :?> string
                      AttIdentity = rdr["attidentity"] :?> string }
                if isDatabaseGenerated att then
                    { Schema = rdr["table_schema"] :?> string
                      Table = rdr["table_name"] :?> string
                      Column = rdr["column_name"] :?> string }
        ]
        |> Set.ofList

    let isReadOnly (col: ColumnSchema) =
        generatedColumns.Contains { Schema = col.Schema; Table = col.Table; Column = col.Name }

    let enums =
        let sql =
            """
            SELECT n.nspname as Schema, t.typname as Enum, e.enumlabel as Label, e.enumsortorder as LabelOrder
            FROM pg_enum e
            JOIN pg_type t ON e.enumtypid = t.oid
            LEFT JOIN   pg_catalog.pg_namespace n ON n.oid = t.typnamespace
            WHERE (t.typrelid = 0 OR (SELECT c.relkind = 'c' FROM pg_catalog.pg_class c WHERE c.oid = t.typrelid)) and typtype = 'e'
                AND NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type el WHERE el.oid = t.typelem AND el.typarray = t.oid)
                AND n.nspname NOT IN ('pg_catalog', 'information_schema');
            """

        use cmd = new Npgsql.NpgsqlCommand(sql, conn)
        use rdr = cmd.ExecuteReader()

        [
            while rdr.Read() do
                {|
                    Schema = rdr["Schema"] :?> string
                    Enum = rdr["Enum"] :?> string
                    Label = rdr["Label"] :?> string
                    LabelOrder = rdr["LabelOrder"] :?> single
                |}
        ]
        |> List.groupBy (fun r -> r.Schema, r.Enum)
        |> List.map (fun (_, grp) ->
            let h = grp |> List.head
            {
                Schema = h.Schema
                Name = h.Enum
                Labels = grp |> List.map (fun r -> { Name = r.Label; SortOrder = System.Convert.ToInt32(r.LabelOrder) })
            }
        )

    let views =
        sViews.Rows
        |> Seq.cast<DataRow>
        |> Seq.map (fun tbl ->
            {|
                Catalog = tbl["TABLE_CATALOG"] :?> string
                Schema = tbl["TABLE_SCHEMA"] :?> string
                Name  = tbl["TABLE_NAME"] :?> string
                Type = "view"
            |}
        )

    let materializedViews =
        sMaterializedViews.Rows
        |> Seq.cast<DataRow>
        |> Seq.map (fun tbl ->
            {|
                Catalog = tbl["TABLE_CATALOG"] :?> string
                Schema = tbl["TABLE_SCHEMA"] :?> string
                Name  = tbl["TABLE_NAME"] :?> string
                Type = "materialized view"
            |}
        )

    let baseTables =
        sTables.Rows
        |> Seq.cast<DataRow>
        |> Seq.map (fun tbl ->
            {|
                Catalog = tbl["TABLE_CATALOG"] :?> string
                Schema = tbl["TABLE_SCHEMA"] :?> string
                Name  = tbl["TABLE_NAME"] :?> string
                Type = tbl["TABLE_TYPE"] :?> string
            |}
        )
        |> Seq.filter (fun tbl -> tbl.Type <> "SYSTEM_TABLE")

    /// Every relation the filters keep. Materialized views go through here too — they used
    /// to bypass filtering entirely, so an excluded one was still generated — and one pass
    /// means the filter summary is reported once.
    let includedRelations =
        baseTables
        |> Seq.append views
        |> Seq.append materializedViews
        |> SchemaFilters.filterTables cfg.Filters
        |> Seq.toList

    /// Every relation's columns, from `pg_catalog`: `information_schema.columns`, which
    /// `GetSchema("Columns")` reads, omits materialized views. The type is spelled as Npgsql's
    /// `GetSchema` spells it, `format_type` of the base type (a domain resolves to what it
    /// wraps), so the type mappings see the same names for a table, a view or a matview.
    let columns =
        let sql =
            """
            SELECT
                current_database() AS table_catalog,
                n.nspname AS table_schema,
                c.relname AS table_name,
                a.attname AS column_name,
                a.attnum AS ordinal_position,
                format_type(COALESCE(NULLIF(t.typbasetype, 0), a.atttypid), NULL) AS data_type,
                a.attnotnull OR (t.typtype = 'd' AND t.typnotnull) AS not_null
            FROM pg_attribute a
            INNER JOIN pg_class c ON c.oid = a.attrelid
            INNER JOIN pg_namespace n ON n.oid = c.relnamespace
            INNER JOIN pg_type t ON t.oid = a.atttypid
            WHERE
                -- tables, views, materialized views, foreign and partitioned tables
                c.relkind IN ('r', 'v', 'm', 'f', 'p') AND
                a.attnum >= 1 AND
                NOT a.attisdropped AND
                n.nspname NOT IN ('pg_catalog', 'information_schema') AND
                has_column_privilege(c.oid, a.attnum, 'SELECT, INSERT, UPDATE, REFERENCES')
            """

        // `GetSchema`'s positional restrictions: catalog, schema, table, column; a null matches all.
        let restrictions = cfg.Filters.TryGetRestrictionsByKey "Columns"
        let restricted (fields: string list) =
            fields |> List.indexed |> List.forall (fun (i, field) ->
                i >= restrictions.Length || isNull restrictions[i] || restrictions[i] = field)

        use cmd = new Npgsql.NpgsqlCommand(sql, conn)
        use rdr = cmd.ExecuteReader()
        [
            while rdr.Read() do
                let catalog = rdr["table_catalog"] :?> string
                let schema = rdr["table_schema"] :?> string
                let table = rdr["table_name"] :?> string
                let name = rdr["column_name"] :?> string
                if restricted [ catalog; schema; table; name ] then
                    {
                        ColumnSchema.Catalog = catalog
                        ColumnSchema.Schema = schema
                        ColumnSchema.Table = table
                        ColumnSchema.Name = name
                        ColumnSchema.ProviderTypeName = rdr["data_type"] :?> string
                        ColumnSchema.Ordinal = rdr["ordinal_position"] :?> int16 |> int
                        ColumnSchema.IsNullable = rdr["not_null"] :?> bool |> not
                        ColumnSchema.Precision = None
                        ColumnSchema.Scale = None
                        ColumnSchema.IsPrimaryKey = pks.Contains(schema, table, name)
                        ColumnSchema.IsComputed = false
                        ColumnSchema.DefaultValue = None
                    }
        ]
        |> List.sortBy _.Ordinal
        |> List.groupBy (fun col -> col.Schema, col.Table)
        |> Map.ofList

    let tryFindTypeMapping =
        let baseTryFind = NpgsqlDataTypes.tryFindTypeMapping isLegacy
        extensions |> List.fold (fun acc (ext: IExtendTypeMapping) -> ext.Extend(acc)) baseTryFind

    let tables =
        includedRelations
        |> Seq.choose (fun tbl ->
            let tableCols = columns |> Map.tryFind (tbl.Schema, tbl.Name) |> Option.defaultValue []

            let tableSchema =
                {
                    TableSchema.Catalog = tbl.Catalog
                    TableSchema.Schema = tbl.Schema
                    TableSchema.Name = tbl.Name
                    TableSchema.Type = if isBaseTableType tbl.Type then TableType.Table else TableType.View
                    TableSchema.Columns = tableCols
                }

            let mappedColumns =
                tableCols
                |> List.choose (fun col ->
                    let ctx = { TypeMappingContext.Table = tableSchema; TypeMappingContext.Column = col }
                    tryFindTypeMapping ctx
                    |> Option.map (fun typeMapping ->
                        {
                            Column.Name = col.Name
                            Column.IsNullable = col.IsNullable
                            Column.TypeMapping = typeMapping
                            Column.IsPK = col.IsPrimaryKey
                            Column.IsReadOnly = isReadOnly col
                        }
                    )
                )

            let enumColumns =
                tableCols
                |> List.choose (fun col ->
                    let fullyQualified = enums |> List.tryFind (fun e -> col.ProviderTypeName = $"{e.Schema}.{e.Name}")
                    let unqualified = enums |> List.tryFind (fun e -> col.ProviderTypeName = e.Name)

                    // The same enum can exist in different schemas.
                    // So ideally, col.ProviderTypeName has a fully qualified enum type (schema.enumName).
                    // If no qualified enum is found, then just use the first unqualified enum.
                    fullyQualified
                    |> Option.orElse unqualified
                    |> Option.map (fun enum -> col, enum)
                )
                |> List.map (fun (col, enum) ->
                    {
                        Column.Name = col.Name
                        Column.IsNullable = col.IsNullable
                        Column.TypeMapping =
                            {
                                TypeMapping.ColumnTypeAlias = col.ProviderTypeName
                                TypeMapping.ClrType =                       // Enum type (will be generated)
                                    if col.Schema <> enum.Schema
                                    then $"{enum.Schema}.{enum.Name}"       // Enum lives in a different schema/module
                                    else enum.Name                          // Enum lives in this module
                                TypeMapping.DbType = DbType.Object
                                TypeMapping.ProviderDbType = None
                            }
                        Column.IsPK = col.IsPrimaryKey
                        Column.IsReadOnly = isReadOnly col
                    }
                )

            let supportedColumns = mappedColumns @ enumColumns

            let filteredColumns =
                supportedColumns
                |> SchemaFilters.filterColumns cfg.Filters tbl.Schema tbl.Name
                |> Seq.toList

            if filteredColumns |> Seq.isEmpty then
                None
            else
                Some {
                    Table.Catalog = tbl.Catalog
                    Table.Schema = tbl.Schema
                    Table.Name =  tbl.Name
                    Table.Type = if isBaseTableType tbl.Type then TableType.Table else TableType.View
                    Table.Columns = filteredColumns
                    Table.TotalColumns = tableCols |> List.length
                }
        )
        |> Seq.toList

    {
        Tables = tables
        Enums = enums
    }
