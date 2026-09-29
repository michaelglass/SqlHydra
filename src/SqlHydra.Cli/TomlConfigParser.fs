module SqlHydra.TomlConfigParser

open System
open System.Collections.Generic
open System.Runtime.Serialization
open Tomlyn
open Tomlyn.Syntax
open Domain

// The shape of a sqlhydra.toml file. Tomlyn maps each snake_case key onto the matching
// PascalCase property (`cli_mutable` -> `CliMutable`); a key left out of the file keeps the
// default declared here. Required keys default to null so `read` can report them by name.

[<AllowNullLiteral>]
type GeneralSection() =
    member val Connection: string = null with get, set
    member val Output: string = null with get, set
    member val Namespace: string = null with get, set
    member val CliMutable = Nullable<bool>() with get, set
    member val MutableProperties = false with get, set
    member val NullablePropertyType = "option" with get, set

[<AllowNullLiteral>]
type ReadersSection() =
    member val ReaderType: string = null with get, set

type FiltersSection() =
    member val Include: string array = [||] with get, set
    member val Exclude: string array = [||] with get, set
    member val Restrictions = Dictionary<string, string array>() with get, set

type ExtensionsSection() =
    member val TypeMappings: string array = [||] with get, set

[<AllowNullLiteral>]
type QueryIntegrationSection() =
    member val ProviderDbTypeAttributes = true with get, set
    /// True when the section exists; `read` turns it off when the section is missing.
    member val TableDeclarations = true with get, set
    /// Absent means false so existing codebases see no change; the init wizard writes
    /// `left_joined_views = true` into new configs.
    member val LeftJoinedViews = false with get, set

type TomlFile() =
    member val General: GeneralSection = null with get, set
    member val Readers: ReadersSection = null with get, set
    member val Filters = FiltersSection() with get, set
    member val Extensions = ExtensionsSection() with get, set
    [<DataMember(Name = "sqlhydra_query_integration")>]
    member val QueryIntegration: QueryIntegrationSection = null with get, set

/// Returns the value of a required key, or fails with a message naming the section and key.
let private required (section: string) (key: string) (value: 'T) =
    if isNull (box value)
    then failwith $"Missing required key '{key}' in the [{section}] section."
    else value

/// Reads .toml file and returns a Config.
let read(toml: string) =

    // NOTE: New configuration keys should be parsed gracefully so as to not break older versions!
    let file = Toml.ToModel<TomlFile>(toml, options = TomlModelOptions(IgnoreMissingProperties = true))
    let general = if isNull file.General then failwith "Missing required section [general]." else file.General
    let queryIntegration =
        match file.QueryIntegration with
        | null -> QueryIntegrationSection(TableDeclarations = false) // No [sqlhydra_query_integration] table: no table declarations
        | section -> section

    {
        Config.ConnectionString = general.Connection |> required "general" "connection"
        Config.OutputFile = general.Output |> required "general" "output"
        Config.Namespace = general.Namespace |> required "general" "namespace"
        Config.IsCLIMutable = (general.CliMutable |> required "general" "cli_mutable").Value
        Config.IsMutableProperties = general.MutableProperties
        Config.NullablePropertyType = 
            match general.NullablePropertyType.ToLower() with
            | "nullable" -> NullablePropertyType.Nullable
            | _ -> NullablePropertyType.Option
        Config.ProviderDbTypeAttributes = queryIntegration.ProviderDbTypeAttributes
        Config.TableDeclarations = queryIntegration.TableDeclarations
        Config.LeftJoinedViews = queryIntegration.LeftJoinedViews
        Config.Readers = 
            match file.Readers with
            | null -> None
            | readers -> Some { ReadersConfig.ReaderType = readers.ReaderType |> required "readers" "reader_type" }
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