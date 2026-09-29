module SqlHydra.TomlConfigParser

open System.Collections.Generic
open System.Text.RegularExpressions
open System.Text.Json
open System.Text.Json.Serialization
open Tomlyn
open Tomlyn.Parsing
open Tomlyn.Serialization
open Tomlyn.Syntax
open Domain

// The shape of a sqlhydra.toml file. Each snake_case key maps onto the matching PascalCase
// property (`cli_mutable` -> `CliMutable`); a key left out of the file keeps the default declared here.

type GeneralSection() =
    [<TomlRequired>] member val Connection = "" with get, set
    [<TomlRequired>] member val Output = "" with get, set
    [<TomlRequired>] member val Namespace = "" with get, set
    [<TomlRequired>] member val CliMutable = false with get, set
    member val MutableProperties = false with get, set
    member val NullablePropertyType = "option" with get, set

[<AllowNullLiteral>]
type ReadersSection() =
    [<TomlRequired>] member val ReaderType = "" with get, set

type FiltersSection() =
    member val Include: string array = [||] with get, set
    member val Exclude: string array = [||] with get, set
    member val Restrictions = Dictionary<string, string array>() with get, set

type ExtensionsSection() =
    member val TypeMappings: string array = [||] with get, set

[<AllowNullLiteral>]
type QueryIntegrationSection() =
    member val ProviderDbTypeAttributes = true with get, set
    member val TableDeclarations = true with get, set
    /// Absent means false so existing codebases see no change; the init wizard writes
    /// `left_joined_views = true` into new configs.
    member val LeftJoinedViews = false with get, set

type TomlFile() =
    [<TomlRequired>] member val General = GeneralSection() with get, set
    member val Readers: ReadersSection = null with get, set
    member val Filters = FiltersSection() with get, set
    member val Extensions = ExtensionsSection() with get, set
    [<JsonPropertyName("sqlhydra_query_integration")>]
    member val QueryIntegration: QueryIntegrationSection = null with get, set

let private options = TomlSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.SnakeCaseLower)

let private keyName (key: KeySyntax) = key.ToString().Trim().Trim('"')

let private withArticle (kind: string) =
    if "AEIOU".Contains kind[0] then $"an {kind}" else $"a {kind}"

/// Finds the section and key of the value at `position`, looking inside inline tables.
let rec private keyAt (section: string) (items: KeyValueSyntax seq) position =
    items |> Seq.tryPick (fun item ->
        let span = item.Span
        if (span.Start.Line, span.Start.Column) <= position && position <= (span.End.Line, span.End.Column) then
            match item.Value with
            | :? InlineTableSyntax as table -> keyAt $"{section}.{keyName item.Key}" (table.Items |> Seq.map (fun i -> i.KeyValue)) position
            | _ -> Some (section, keyName item.Key)
        else None)

/// Rewords a Tomlyn error to name the section and key. Tomlyn's message names the type that is
/// missing a required key, and its position points at a value of the wrong type.
let private describe (toml: string) (ex: TomlException) =
    let missing = Regex.Match(ex.Message, @"Missing required TOML key '(\w+)' when deserializing '.*\+(\w+)'")
    let wrongType = Regex.Match(ex.Message, @"Expected (\w+) token but was (\w+)")
    let kind (m: Match) (i: int) = withArticle (m.Groups[i].Value.Replace("Start", "")) // StartArray -> Array
    if missing.Success then
        let section =
            match missing.Groups[2].Value with
            | "TomlFile" -> "The config"
            | owner -> "[" + JsonNamingPolicy.SnakeCaseLower.ConvertName(owner.Replace("Section", "")) + "]"
        $"{section} is missing required key '{missing.Groups[1].Value}'."
    elif wrongType.Success && ex.Line.HasValue then
        let position = (ex.Line.Value - 1, ex.Column.Value - 1)
        match SyntaxParser.Parse(toml).Tables |> Seq.tryPick (fun table -> keyAt (keyName table.Name) table.Items position) with
        | Some (section, key) -> $"[{section}] key '{key}' should be {kind wrongType 1}, but is {kind wrongType 2}."
        | None -> ex.Message
    else ex.Message

/// Reads .toml file and returns a Config.
let read(toml: string) =

    // NOTE: New configuration keys should be parsed gracefully so as to not break older versions!
    let file =
        try TomlSerializer.Deserialize<TomlFile>(toml, options)
        with :? TomlException as ex -> failwith (describe toml ex)
    let general = file.General
    let queryIntegration = Option.ofObj file.QueryIntegration

    {
        Config.ConnectionString = general.Connection
        Config.OutputFile = general.Output
        Config.Namespace = general.Namespace
        Config.IsCLIMutable = general.CliMutable
        Config.IsMutableProperties = general.MutableProperties
        Config.NullablePropertyType = 
            match general.NullablePropertyType.ToLower() with
            | "nullable" -> NullablePropertyType.Nullable
            | _ -> NullablePropertyType.Option
        // Without a [sqlhydra_query_integration] section, provider attributes are on and table declarations off.
        Config.ProviderDbTypeAttributes = queryIntegration |> Option.forall (fun section -> section.ProviderDbTypeAttributes)
        Config.TableDeclarations = queryIntegration |> Option.exists (fun section -> section.TableDeclarations)
        Config.LeftJoinedViews = queryIntegration |> Option.exists (fun section -> section.LeftJoinedViews)
        Config.Readers = file.Readers |> Option.ofObj |> Option.map (fun readers -> { ReadersConfig.ReaderType = readers.ReaderType })
        Config.TypeMappingExtensions = file.Extensions.TypeMappings |> Array.toList
        Config.Filters =
            {
                Filters.Includes = file.Filters.Include |> Array.toList
                Filters.Excludes = file.Filters.Exclude |> Array.toList
                Filters.Restrictions = 
                    file.Filters.Restrictions
                    |> Seq.map (fun kvp -> 
                        kvp.Key, 
                            kvp.Value 
                            |> Array.map (fun s -> if s = "" then null else s) // GetSchema expects nulls for missing values, not empty strings.
                    )
                    |> Map.ofSeq
            }
    }

/// Saves a Config to .toml file.
let save(cfg: Config) =
    let doc = DocumentSyntax()
    
    let general = TableSyntax("general")        
    general.Items.Add("connection", cfg.ConnectionString)
    general.Items.Add("output", cfg.OutputFile)
    general.Items.Add("namespace", cfg.Namespace)
    general.Items.Add("cli_mutable", cfg.IsCLIMutable)
    doc.Tables.Add(general)
    
    let queryInt = TableSyntax("sqlhydra_query_integration")
    queryInt.Items.Add("provider_db_type_attributes", cfg.ProviderDbTypeAttributes)
    queryInt.Items.Add("table_declarations", cfg.TableDeclarations)
    queryInt.Items.Add("left_joined_views", cfg.LeftJoinedViews)
    doc.Tables.Add(queryInt)

    cfg.Readers |> Option.iter (fun readersConfig ->
        let readers = TableSyntax("readers")
        readers.Items.Add("reader_type", readersConfig.ReaderType)
        doc.Tables.Add(readers))

    if cfg.TypeMappingExtensions <> [] then
        let extensions = TableSyntax("extensions")
        extensions.Items.Add("type_mappings", cfg.TypeMappingExtensions |> List.toArray)
        doc.Tables.Add(extensions)

    let filters = TableSyntax("filters")
    filters.Items.Add("include", cfg.Filters.Includes |> List.toArray)
    filters.Items.Add("exclude", cfg.Filters.Excludes |> List.toArray)

    doc.Tables.Add(filters)
    
    let toml = doc.ToString()
    toml