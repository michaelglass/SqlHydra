/// Implementations for OnConflict and InsertOrReplace.
module internal SqlHydra.Query.OnConflict

open System

/// Modifies an insert query to "INSERT OR REPLACE"
let insertOrReplace (cmdText: string) =
    cmdText.Replace("INSERT", "INSERT OR REPLACE")

/// Separates an insert query from its optional trailing identity query (split on ";").
let private splitInsertQuery (cmdText: string) =
    match cmdText.Split([| ";" |], StringSplitOptions.RemoveEmptyEntries) with
    | [| insertQuery; identityQuery |] -> insertQuery, identityQuery
    | _ -> cmdText, ""

/// Renders the conflict target clause: ON CONFLICT(...)
let private renderTarget (target: ConflictTarget) =
    match target with
    | TypedColumns columns ->
        let csv = String.Join(",", columns)
        $"ON CONFLICT({csv})"
    | TypedColumnsWhereRaw (columns, whereClause) ->
        let csv = String.Join(",", columns)
        $"ON CONFLICT({csv}) WHERE {whereClause}"
    | RawTarget rawTarget ->
        $"ON CONFLICT({rawTarget})"

/// Renders the conflict action clause: DO NOTHING / DO UPDATE SET ...
let private renderAction (table: string) (action: ConflictAction) =
    match action with
    | DoNothing -> "DO NOTHING"
    | DoUpdate updateFields ->
        let setLines =
            updateFields
            |> List.map (fun colNm -> $"{colNm}=EXCLUDED.\"{colNm}\"")
            |> (fun lines -> String.Join(",\n", lines))
        $"DO UPDATE SET\n{setLines}"
    | DoUpdateCoalesce (updateFields, coalesceFields) ->
        let coalesceSet = Set.ofList coalesceFields
        let quotedTable =
            table.Split('.')
            |> Array.map (fun part -> $"\"{part}\"")
            |> (fun parts -> String.Join(".", parts))
        let setLines =
            updateFields
            |> List.map (fun colNm ->
                if coalesceSet.Contains colNm then
                    $"{colNm}=COALESCE(EXCLUDED.\"{colNm}\", {quotedTable}.\"{colNm}\")"
                else
                    $"{colNm}=EXCLUDED.\"{colNm}\""
            )
            |> (fun lines -> String.Join(",\n", lines))
        $"DO UPDATE SET\n{setLines}"

/// Applies a conflict target + action to a compiled INSERT command text.
let applyConflict (table: string) (target: ConflictTarget) (action: ConflictAction) (cmdText: string) =
    let insertQuery, identityQuery = splitInsertQuery cmdText
    Text.StringBuilder()
        .AppendLine(insertQuery)
        .AppendLine(renderTarget target)
        .AppendLine(renderAction table action).Append(";")
        .AppendLine(identityQuery)
        .ToString()
