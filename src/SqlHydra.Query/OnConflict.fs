/// Implementations for OnConflictDoUpdate, OnConflictDoNothing and InsertOrReplace.
module internal SqlHydra.Query.OnConflict

open System

/// Modifies an insert query to "INSERT OR REPLACE"
let insertOrReplace (cmdText: string) =
    cmdText.Replace("INSERT", "INSERT OR REPLACE")

/// Modifies an insert query to "ON CONFLICT TO UPDATE"
let onConflictDoUpdate (conflictColumns: string list) (updateColumns: string list) (cmdText: string) =
    // Separate insert query from optional identity query
    let insertQuery, identityQuery = 
        match cmdText.Split([| ";" |], StringSplitOptions.RemoveEmptyEntries) with
        | [| insertQuery; identityQuery |] -> insertQuery, identityQuery
        | _ -> cmdText, ""

    // Build upsert clause
    let setLinesStatement = 
        updateColumns
        |> List.map (fun colNm -> $"{colNm}=EXCLUDED.\"{colNm}\"\n")
        |> (fun lines -> String.Join(",", lines))
            
    let conflictColumnsCsv = String.Join(",", conflictColumns)

    Text.StringBuilder()
        .AppendLine(insertQuery)
        .AppendLine($"ON CONFLICT({conflictColumnsCsv}) DO UPDATE SET")
        .AppendLine(setLinesStatement).Append(";")
        .AppendLine(identityQuery)
        .ToString()

/// Modifies an insert query to "ON CONFLICT DO UPDATE" with COALESCE for specified columns
let onConflictDoUpdateCoalesce (table: string) (conflictColumns: string list) (updateColumns: string list) (coalesceColumns: string list) (cmdText: string) =
    let insertQuery, identityQuery =
        match cmdText.Split([| ";" |], StringSplitOptions.RemoveEmptyEntries) with
        | [| insertQuery; identityQuery |] -> insertQuery, identityQuery
        | _ -> cmdText, ""

    let coalesceSet = Set.ofList coalesceColumns

    // Quote the table name parts for use in column qualification
    let quotedTable =
        table.Split('.')
        |> Array.map (fun part -> $"\"{part}\"")
        |> (fun parts -> String.Join(".", parts))

    let setLinesStatement =
        updateColumns
        |> List.map (fun colNm ->
            if coalesceSet.Contains colNm then
                $"{colNm}=COALESCE(EXCLUDED.\"{colNm}\", {quotedTable}.\"{colNm}\")\n"
            else
                $"{colNm}=EXCLUDED.\"{colNm}\"\n"
        )
        |> (fun lines -> String.Join(",", lines))

    let conflictColumnsCsv = String.Join(",", conflictColumns)

    Text.StringBuilder()
        .AppendLine(insertQuery)
        .AppendLine($"ON CONFLICT({conflictColumnsCsv}) DO UPDATE SET")
        .AppendLine(setLinesStatement).Append(";")
        .AppendLine(identityQuery)
        .ToString()

/// Modifies an insert query to "ON CONFLICT TO NOTHING"
let onConflictDoNothing (conflictColumns: string list) (cmdText: string) =
    // Separate insert query from optional identity query
    let insertQuery, identityQuery =
        match cmdText.Split([| ";" |], StringSplitOptions.RemoveEmptyEntries) with
        | [| insertQuery; identityQuery |] -> insertQuery, identityQuery
        | _ -> cmdText, ""

    // Build upsert clause
    let conflictColumnsCsv = String.Join(",", conflictColumns)

    Text.StringBuilder()
        .AppendLine(insertQuery)
        .AppendLine($"ON CONFLICT({conflictColumnsCsv})")
        .AppendLine("DO NOTHING;")
        .AppendLine(identityQuery)
        .ToString()

/// Modifies an insert query to "ON CONFLICT ... WHERE ... DO NOTHING"
let onConflictDoNothingWhere (conflictColumns: string list) (whereClause: string) (cmdText: string) =
    // Separate insert query from optional identity query
    let insertQuery, identityQuery =
        match cmdText.Split([| ";" |], StringSplitOptions.RemoveEmptyEntries) with
        | [| insertQuery; identityQuery |] -> insertQuery, identityQuery
        | _ -> cmdText, ""

    let conflictColumnsCsv = String.Join(",", conflictColumns)

    Text.StringBuilder()
        .AppendLine(insertQuery)
        .AppendLine($"ON CONFLICT({conflictColumnsCsv}) WHERE {whereClause}")
        .AppendLine("DO NOTHING;")
        .AppendLine(identityQuery)
        .ToString()


