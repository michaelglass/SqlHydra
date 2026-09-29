module SqlHydra.TomlConfigParser

open System
open System.Collections.Generic
open System.Reflection
open System.Runtime.Serialization
open Tomlyn
open Tomlyn.Helpers
open Tomlyn.Model
open Tomlyn.Syntax
open Domain

/// Marks a key that must be present in its section.
type RequiredAttribute() = inherit Attribute()

// The shape of a sqlhydra.toml file. Each snake_case key maps onto the matching PascalCase
// property (`cli_mutable` -> `CliMutable`); a key left out of the file keeps the default declared here.

type GeneralSection() =
    [<Required>] member val Connection = "" with get, set
    [<Required>] member val Output = "" with get, set
    [<Required>] member val Namespace = "" with get, set
    [<Required>] member val CliMutable = false with get, set
    member val MutableProperties = false with get, set
    member val NullablePropertyType = "option" with get, set

[<AllowNullLiteral>]
type ReadersSection() =
    [<Required>] member val ReaderType = "" with get, set

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
    [<Required>] member val General = GeneralSection() with get, set
    member val Readers: ReadersSection = null with get, set
    member val Filters = FiltersSection() with get, set
    member val Extensions = ExtensionsSection() with get, set
    [<DataMember(Name = "sqlhydra_query_integration")>]
    member val QueryIntegration: QueryIntegrationSection = null with get, set

/// The TOML key for a model property, named the way Tomlyn maps it.
let private keyOf (prop: PropertyInfo) =
    match prop.GetCustomAttribute<DataMemberAttribute>() with
    | null -> TomlNamingHelper.PascalToSnakeCase prop.Name
    | dataMember -> dataMember.Name

let private kindOfType (t: Type) =
    if t = typeof<string> then "String"
    elif t = typeof<bool> then "Boolean"
    elif t.IsArray then "Array"
    else "Table"

let private kindOfValue (value: obj) =
    match value with
    | :? string -> "String"
    | :? bool -> "Boolean"
    | :? int64 -> "Integer"
    | :? double -> "Float"
    | :? TomlArray -> "Array"
    | :? TomlTable -> "Table"
    | _ -> value.GetType().Name

let private describe (path: string) =
    if path = "" then "The config" else $"[{path}]"

let private withArticle (kind: string) =
    if "AEIOU".Contains kind[0] then $"an {kind}" else $"a {kind}"

/// Checks a parsed TOML table against the model type that Tomlyn will map it onto, so a missing
/// required key or a mistyped value fails naming its section and key. `path` is "" for the whole file.
let rec private checkTable (path: string) (model: Type) (table: TomlTable) =
    for prop in model.GetProperties() do
        let key = keyOf prop
        match table.TryGetValue key with
        | true, value -> checkValue path key prop.PropertyType value
        | _ when prop.IsDefined(typeof<RequiredAttribute>) ->
            failwith $"{describe path} is missing required key '{key}'."
        | _ -> ()

and private checkValue (path: string) (key: string) (expected: Type) (value: obj) =
    let expectedKind, actualKind = kindOfType expected, kindOfValue value
    if expectedKind <> actualKind then
        failwith $"{describe path} key '{key}' should be {withArticle expectedKind}, but is {withArticle actualKind}."
    let childPath = if path = "" then key else $"{path}.{key}"
    match value with
    | :? TomlArray as items -> 
        for item in items do checkValue path key (expected.GetElementType()) item
    | :? TomlTable as table when expected.IsGenericType -> // A dictionary, e.g. [filters.restrictions]
        for kvp in table do checkValue childPath kvp.Key (expected.GetGenericArguments().[1]) kvp.Value
    | :? TomlTable as table -> 
        checkTable childPath expected table
    | _ -> ()

/// Reads .toml file and returns a Config.
let read(toml: string) =

    // NOTE: New configuration keys should be parsed gracefully so as to not break older versions!
    checkTable "" typeof<TomlFile> (Toml.ToModel toml)
    let file = Toml.ToModel<TomlFile>(toml, options = TomlModelOptions(IgnoreMissingProperties = true))
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