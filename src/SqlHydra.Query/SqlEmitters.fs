namespace SqlHydra.Query

open System
open System.Text
open SqlHydra.Domain

// ─── SQL Server Emitter ───

type SqlServerEmitter() =
    inherit SqlEmitterBase()

    override _.QuoteIdentifier(name) = $"[{name}]"
    override _.ParameterPrefix = "@p"

    /// SQL Server pagination: uses OFFSET/FETCH when ORDER BY is present, TOP when not.
    /// Note: this is called after ORDER BY is already appended to sb.
    override this.EmitPagination(skip, take, sb, collector) =
        let hasOrderBy = sb.ToString().Contains("ORDER BY")
        match skip, take with
        | Some s, Some t when hasOrderBy ->
            let skipParam = collector.Add(box s)
            let takeParam = collector.Add(box t)
            sb.Append($" OFFSET {skipParam} ROWS FETCH NEXT {takeParam} ROWS ONLY") |> ignore
        | None, Some t when hasOrderBy ->
            let skipParam = collector.Add(box 0)
            let takeParam = collector.Add(box t)
            sb.Append($" OFFSET {skipParam} ROWS FETCH NEXT {takeParam} ROWS ONLY") |> ignore
        | Some s, None when hasOrderBy ->
            let skipParam = collector.Add(box s)
            sb.Append($" OFFSET {skipParam} ROWS") |> ignore
        | None, Some t ->
            // No ORDER BY: use TOP N (insert after SELECT keyword)
            let sql = sb.ToString()
            let selectIdx = sql.IndexOf("SELECT ", StringComparison.OrdinalIgnoreCase)
            if selectIdx >= 0 then
                let insertAt = selectIdx + "SELECT ".Length
                // Check for DISTINCT
                let afterSelect = sql.Substring(insertAt)
                let actualInsert =
                    if afterSelect.StartsWith("DISTINCT ", StringComparison.OrdinalIgnoreCase)
                    then insertAt + "DISTINCT ".Length
                    else insertAt
                let topClause = $"TOP ({t}) "
                sb.Clear() |> ignore
                sb.Append(sql.Insert(actualInsert, topClause)) |> ignore
        | _ -> ()

    // SQL Server emits boolean values as cast(x as bit) inline
    override _.EmitBoolColumn(quotedCol, value, _collector) =
        let bitVal = if value then "1" else "0"
        $"{quotedCol} = cast({bitVal} as bit)"

    override _.EmitInsertIdentity(_field) =
        ";SELECT scope_identity() as Id"

    override _.EmitInsertOutput(outputFields, insertSql) =
        let outputCsv =
            outputFields
            |> List.map (fun f -> $"INSERTED.{f.ColumnName}")
            |> String.concat ", "
        let outputClause = $"\nOUTPUT {outputCsv}\n"
        let valuesIndex = insertSql.IndexOf("VALUES", StringComparison.OrdinalIgnoreCase)
        if valuesIndex > -1 then
            insertSql.Insert(valuesIndex, outputClause)
        else
            insertSql + outputClause

    override _.EmitUpdateOutput(outputFields, updateSql) =
        let outputCsv =
            outputFields
            |> List.map (fun f -> $"INSERTED.{f.ColumnName}")
            |> String.concat ", "
        let outputClause = $"\nOUTPUT {outputCsv}\n"
        let whereIndex = updateSql.IndexOf("WHERE", StringComparison.OrdinalIgnoreCase)
        if whereIndex > -1 then
            updateSql.Insert(whereIndex, outputClause)
        else
            updateSql + outputClause

    interface ISqlEmitter with
        member _.Provider = SqlServer
        member this.EmitSelect(ir) = this.EmitSelectCore(ir)
        member this.EmitInsert(ir) = this.EmitInsertCore(ir)
        member this.EmitUpdate(ir) = this.EmitUpdateCore(ir)
        member this.EmitDelete(ir) = this.EmitDeleteCore(ir)

// ─── PostgreSQL Emitter ───

type PostgresEmitter() =
    inherit SqlEmitterBase()

    override _.QuoteIdentifier(name) = $"\"{name}\""
    override _.ParameterPrefix = "@p"

    override _.EmitInsertIdentity(field) =
        $" RETURNING \"{field}\";"

    // Postgres uses case-insensitive ilike
    override _.EmitLike(quotedCol, paramName) = $"{quotedCol} ilike {paramName}"
    override _.EmitNotLike(quotedCol, paramName) = $"NOT ({quotedCol} ilike {paramName})"

    override _.EmitReturning(returning, sql) =
        if returning.Length = 0 then sql
        else
            let cols = returning |> List.map (fun c -> $"\"{c}\"") |> String.concat ", "
            // If sql ended with a trailing ";", insert RETURNING before it.
            let trimmed = sql.TrimEnd()
            if trimmed.EndsWith(";") then
                let body = trimmed.Substring(0, trimmed.Length - 1)
                $"{body} RETURNING {cols};"
            else
                $"{sql} RETURNING {cols}"

    override this.EmitInsertConflict(insertType, insertSql, columns, _rows, collector) =
        let split (sql: string) =
            match sql.Split([| ";" |], StringSplitOptions.RemoveEmptyEntries) with
            | [| iq; idq |] -> iq, idq
            | _ -> sql, ""

        match insertType with
        | OnConflictDoUpdate (conflictFields, updateFields) ->
            let insertQuery, identityQuery = split insertSql
            let setLines =
                updateFields
                |> List.map (fun col -> $"{col}=EXCLUDED.\"{col}\"\n")
                |> fun lines -> String.Join(",", lines)
            let conflictCsv = String.Join(",", conflictFields)
            StringBuilder()
                .AppendLine(insertQuery)
                .AppendLine($"ON CONFLICT({conflictCsv}) DO UPDATE SET")
                .AppendLine(setLines).Append(";")
                .AppendLine(identityQuery)
                .ToString()

        | OnConflictDoUpdateCoalesce (conflictFields, updateFields) ->
            let insertQuery, identityQuery = split insertSql
            let setLines =
                updateFields
                |> List.map (fun col -> $"\"{col}\" = COALESCE(EXCLUDED.\"{col}\", \"{col}\")\n")
                |> fun lines -> String.Join(",", lines)
            let conflictCsv = String.Join(",", conflictFields)
            StringBuilder()
                .AppendLine(insertQuery)
                .AppendLine($"ON CONFLICT({conflictCsv}) DO UPDATE SET")
                .AppendLine(setLines).Append(";")
                .AppendLine(identityQuery)
                .ToString()

        | OnConflictDoNothing conflictFields ->
            let insertQuery, identityQuery = split insertSql
            let conflictCsv = String.Join(",", conflictFields)
            StringBuilder()
                .AppendLine(insertQuery)
                .AppendLine($"ON CONFLICT({conflictCsv})")
                .AppendLine("DO NOTHING;")
                .AppendLine(identityQuery)
                .ToString()

        | OnConflictDoNothingWhereRaw (conflictFields, whereFragment, parms) ->
            let insertQuery, identityQuery = split insertSql
            let conflictCsv = String.Join(",", conflictFields)
            // Substitute ? placeholders in whereFragment with newly added parameters
            let mutable rendered = whereFragment
            for p in parms do
                let name = collector.Add(p)
                let idx = rendered.IndexOf("?")
                if idx >= 0 then
                    rendered <- rendered.Substring(0, idx) + name + rendered.Substring(idx + 1)
            StringBuilder()
                .AppendLine(insertQuery)
                .AppendLine($"ON CONFLICT({conflictCsv}) WHERE {rendered}")
                .AppendLine("DO NOTHING;")
                .AppendLine(identityQuery)
                .ToString()

        | OnConflictDoNothingRawTarget rawTarget ->
            let insertQuery, identityQuery = split insertSql
            StringBuilder()
                .AppendLine(insertQuery)
                .AppendLine($"ON CONFLICT({rawTarget})")
                .AppendLine("DO NOTHING;")
                .AppendLine(identityQuery)
                .ToString()

        | _ -> insertSql

    interface ISqlEmitter with
        member _.Provider = Npgsql
        member this.EmitSelect(ir) = this.EmitSelectCore(ir)
        member this.EmitInsert(ir) = this.EmitInsertCore(ir)
        member this.EmitUpdate(ir) = this.EmitUpdateCore(ir)
        member this.EmitDelete(ir) = this.EmitDeleteCore(ir)

// ─── SQLite Emitter ───

type SqliteEmitter() =
    inherit SqlEmitterBase()

    override _.QuoteIdentifier(name) = $"\"{name}\""
    override _.ParameterPrefix = "@p"

    override _.EmitInsertIdentity(_field) =
        ";select last_insert_rowid() as id"

    override _.EmitReturning(returning, sql) =
        if returning.Length = 0 then sql
        else
            let cols = returning |> List.map (fun c -> $"\"{c}\"") |> String.concat ", "
            let trimmed = sql.TrimEnd()
            if trimmed.EndsWith(";") then
                let body = trimmed.Substring(0, trimmed.Length - 1)
                $"{body} RETURNING {cols};"
            else
                $"{sql} RETURNING {cols}"

    override this.EmitInsertConflict(insertType, insertSql, _columns, _rows, _collector) =
        let split (sql: string) =
            match sql.Split([| ";" |], StringSplitOptions.RemoveEmptyEntries) with
            | [| iq; idq |] -> iq, idq
            | _ -> sql, ""

        match insertType with
        | InsertOrReplace ->
            insertSql.Replace("INSERT", "INSERT OR REPLACE")

        | OnConflictDoUpdate (conflictFields, updateFields) ->
            let insertQuery, identityQuery = split insertSql
            let setLines =
                updateFields
                |> List.map (fun col -> $"{col}=EXCLUDED.\"{col}\"\n")
                |> fun lines -> String.Join(",", lines)
            let conflictCsv = String.Join(",", conflictFields)
            StringBuilder()
                .AppendLine(insertQuery)
                .AppendLine($"ON CONFLICT({conflictCsv}) DO UPDATE SET")
                .AppendLine(setLines).Append(";")
                .AppendLine(identityQuery)
                .ToString()

        | OnConflictDoUpdateCoalesce (conflictFields, updateFields) ->
            let insertQuery, identityQuery = split insertSql
            let setLines =
                updateFields
                |> List.map (fun col -> $"\"{col}\" = COALESCE(EXCLUDED.\"{col}\", \"{col}\")\n")
                |> fun lines -> String.Join(",", lines)
            let conflictCsv = String.Join(",", conflictFields)
            StringBuilder()
                .AppendLine(insertQuery)
                .AppendLine($"ON CONFLICT({conflictCsv}) DO UPDATE SET")
                .AppendLine(setLines).Append(";")
                .AppendLine(identityQuery)
                .ToString()

        | OnConflictDoNothing conflictFields ->
            let insertQuery, identityQuery = split insertSql
            let conflictCsv = String.Join(",", conflictFields)
            StringBuilder()
                .AppendLine(insertQuery)
                .AppendLine($"ON CONFLICT({conflictCsv})")
                .AppendLine("DO NOTHING;")
                .AppendLine(identityQuery)
                .ToString()

        | _ -> insertSql

    interface ISqlEmitter with
        member _.Provider = Sqlite
        member this.EmitSelect(ir) = this.EmitSelectCore(ir)
        member this.EmitInsert(ir) = this.EmitInsertCore(ir)
        member this.EmitUpdate(ir) = this.EmitUpdateCore(ir)
        member this.EmitDelete(ir) = this.EmitDeleteCore(ir)

// ─── Oracle Emitter ───

type OracleEmitter() =
    inherit SqlEmitterBase()

    override _.QuoteIdentifier(name) = $"\"{name}\""
    override _.ParameterPrefix = ":p"

    // Oracle doesn't use AS for table aliases
    override this.QuoteTableSpec(spec: string) =
        let parts = spec.Split([| " as "; " AS "; " As " |], StringSplitOptions.RemoveEmptyEntries)
        match parts with
        | [| table; alias |] ->
            $"{this.QuoteDotted(table.Trim())} {this.QuoteIdentifier(alias.Trim())}"
        | [| table |] ->
            this.QuoteDotted(table.Trim())
        | _ -> spec

    override this.EmitPagination(skip, take, sb, collector) =
        match skip with
        | Some s ->
            let paramName = collector.Add(box s)
            sb.Append($" OFFSET {paramName} ROWS") |> ignore
        | None -> ()
        match take with
        | Some t ->
            let paramName = collector.Add(box t)
            sb.Append($" FETCH FIRST {paramName} ROWS ONLY") |> ignore
        | None -> ()

    override _.EmitInsertIdentity(field) =
        $" returning \"{field}\" into :outputParam"

    override this.EmitMultiRowInsert(table, columns, rows, collector) =
        // Oracle INSERT ALL syntax
        let quotedTable = this.QuoteDotted(table)
        let quotedCols = columns |> List.map this.QuoteIdentifier |> String.concat ", "
        let sb = StringBuilder()
        sb.AppendLine("INSERT ALL") |> ignore
        for row in rows do
            let paramNames = row |> Array.map (fun v -> collector.Add(v)) |> String.concat ", "
            sb.AppendLine($"INTO {quotedTable} ({quotedCols}) VALUES ({paramNames})") |> ignore
        sb.AppendLine("SELECT * FROM DUAL") |> ignore
        sb.ToString()

    interface ISqlEmitter with
        member _.Provider = Oracle
        member this.EmitSelect(ir) = this.EmitSelectCore(ir)
        member this.EmitInsert(ir) = this.EmitInsertCore(ir)
        member this.EmitUpdate(ir) = this.EmitUpdateCore(ir)
        member this.EmitDelete(ir) = this.EmitDeleteCore(ir)

// ─── MySQL Emitter ───

type MySqlEmitter() =
    inherit SqlEmitterBase()

    override _.QuoteIdentifier(name) = $"`{name}`"
    override _.ParameterPrefix = "@p"

    interface ISqlEmitter with
        member _.Provider = MySql
        member this.EmitSelect(ir) = this.EmitSelectCore(ir)
        member this.EmitInsert(ir) = this.EmitInsertCore(ir)
        member this.EmitUpdate(ir) = this.EmitUpdateCore(ir)
        member this.EmitDelete(ir) = this.EmitDeleteCore(ir)
