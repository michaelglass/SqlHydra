module UnitTests.``TOML Config Parser``

open System
open SqlHydra
open SqlHydra.Domain
open NUnit.Framework
open Swensen.Unquote
open System.Globalization

/// Compare two strings ignoring white space and line breaks
let assertEqual (s1: string, s2: string) = 
    Assert.IsTrue (String.Compare(s1, s2, CultureInfo.CurrentCulture, CompareOptions.IgnoreCase ||| CompareOptions.IgnoreSymbols) = 0)

[<Test>]
let ``Save: All``() = 
    let cfg = 
        {
            ConnectionString = @"Data Source=localhost\SQLEXPRESS;Initial Catalog=AdventureWorksLT2019;Integrated Security=SSPI"
            OutputFile = "AdventureWorks.fs"
            Namespace = "SampleApp.AdventureWorks"
            IsCLIMutable = true
            IsMutableProperties = false
            NullablePropertyType = NullablePropertyType.Option
            ProviderDbTypeAttributes = true
            TableDeclarations = true
            LeftJoinedViews = true
            Readers = Some { ReadersConfig.ReaderType = "Microsoft.Data.SqlClient.SqlDataReader" }
            Filters = Filters.Empty
            TypeMappingExtensions = []
        }

    let toml = TomlConfigParser.save(cfg)

    let expected = 
        """
        [general]
        connection = "Data Source=localhost\\SQLEXPRESS;Initial Catalog=AdventureWorksLT2019;Integrated Security=SSPI"
        output = "AdventureWorks.fs"
        namespace = "SampleApp.AdventureWorks"
        cli_mutable = true
        [sqlhydra_query_integration]
        provider_db_type_attributes = true
        table_declarations = true
        left_joined_views = true
        [readers]
        reader_type = "Microsoft.Data.SqlClient.SqlDataReader"
        [filters]
        include = []
        exclude = []
        """

    assertEqual(expected, toml)

[<Test>]
let ``Read: with no filters``() = 
    let toml = 
        """
        [general]
        connection = "Data Source=localhost\\SQLEXPRESS;Initial Catalog=AdventureWorksLT2019;Integrated Security=SSPI"
        output = "AdventureWorks.fs"
        namespace = "SampleApp.AdventureWorks"
        cli_mutable = true
        [readers]
        reader_type = "Microsoft.Data.SqlClient.SqlDataReader"
        """

    let expected = 
        {
            ConnectionString = @"Data Source=localhost\SQLEXPRESS;Initial Catalog=AdventureWorksLT2019;Integrated Security=SSPI"
            OutputFile = "AdventureWorks.fs"
            Namespace = "SampleApp.AdventureWorks"
            IsCLIMutable = true
            IsMutableProperties = false
            NullablePropertyType = NullablePropertyType.Option
            ProviderDbTypeAttributes = true
            TableDeclarations = false
            LeftJoinedViews = false
            Readers = Some { ReadersConfig.ReaderType = "Microsoft.Data.SqlClient.SqlDataReader" }
            Filters = Filters.Empty
            TypeMappingExtensions = []
        }

    let cfg = TomlConfigParser.read(toml)

    cfg =! expected

[<Test>]
let ``Read: when no readers section should be None``() = 
    let toml = 
        """
        [general]
        connection = "Data Source=localhost\\SQLEXPRESS;Initial Catalog=AdventureWorksLT2019;Integrated Security=SSPI"
        output = "AdventureWorks.fs"
        namespace = "SampleApp.AdventureWorks"
        cli_mutable = true
        """

    let expected = 
        {
            ConnectionString = @"Data Source=localhost\SQLEXPRESS;Initial Catalog=AdventureWorksLT2019;Integrated Security=SSPI"
            OutputFile = "AdventureWorks.fs"
            Namespace = "SampleApp.AdventureWorks"
            IsCLIMutable = true
            IsMutableProperties = false
            NullablePropertyType = NullablePropertyType.Option
            ProviderDbTypeAttributes = true
            TableDeclarations = false
            LeftJoinedViews = false
            Readers = None
            Filters = Filters.Empty
            TypeMappingExtensions = []
        }

    let cfg = TomlConfigParser.read(toml)

    cfg =! expected

[<Test>]
let ``Read: should parse filters``() = 
    let toml = 
        """
        [general]
        connection = "Data Source=localhost\\SQLEXPRESS;Initial Catalog=AdventureWorksLT2019;Integrated Security=SSPI"
        output = "AdventureWorks.fs"
        namespace = "SampleApp.AdventureWorks"
        cli_mutable = true
        [filters]
        include = [ "products/*", "dbo/*" ]
        exclude = [ "products/system*" ]                
        """

    let expectedFilters =
        { 
            Includes = [ "products/*"; "dbo/*" ]
            Excludes = [ "products/system*" ] 
            Restrictions = Map.empty
        }

    let cfg = TomlConfigParser.read(toml)

    cfg.Filters =! expectedFilters

[<Test>]
let ``Read: should parse schema restrictions``() = 
    let toml = 
        """
        [general]
        connection = "Data Source=localhost\\SQLEXPRESS;Initial Catalog=AdventureWorksLT2019;Integrated Security=SSPI"
        output = "AdventureWorks.fs"
        namespace = "SampleApp.AdventureWorks"
        cli_mutable = true
        [filters]
        include = []
        exclude = []
        restrictions = { "Tables" = [ "products" ], "Columns" = [ "", "Price" ] }
        """

    let expectedFilters =
        { 
            Includes = []
            Excludes = [] 
            Restrictions = 
                Map [ 
                    "Tables", [| "products" |]
                    "Columns", [| null; "Price" |] 
                ]
        }

    let cfg = TomlConfigParser.read(toml)

    cfg.Filters =! expectedFilters

let private readFilters (filters: string) =
    let toml =
        $"""
        [general]
        connection = "Data Source=localhost"
        output = "AdventureWorks.fs"
        namespace = "SampleApp.AdventureWorks"
        cli_mutable = true
        [filters]
        {filters}
        """
    (TomlConfigParser.read toml).Filters

[<Test>]
let ``Read: filters with include but no exclude``() =
    readFilters """include = [ "dbo/*" ]""" =! { Filters.Empty with Includes = [ "dbo/*" ] }

[<Test>]
let ``Read: filters with exclude but no include``() =
    readFilters """exclude = [ "dbo/temp*" ]""" =! { Filters.Empty with Excludes = [ "dbo/temp*" ] }

[<Test>]
let ``Read: filters with only restrictions``() =
    readFilters """restrictions = { "Tables" = [ "products" ] }"""
    =! { Filters.Empty with Restrictions = Map [ "Tables", [| "products" |] ] }

[<Test>]
let ``Read: query integration without provider_db_type_attributes defaults to true``() =
    let toml =
        """
        [general]
        connection = "Data Source=localhost"
        output = "AdventureWorks.fs"
        namespace = "SampleApp.AdventureWorks"
        cli_mutable = true
        [sqlhydra_query_integration]
        table_declarations = true
        """
    (TomlConfigParser.read toml).ProviderDbTypeAttributes =! true

let private general =
    """
    [general]
    connection = "Data Source=localhost"
    output = "AdventureWorks.fs"
    namespace = "SampleApp.AdventureWorks"
    cli_mutable = true
    """

/// Reads a config that should be rejected and returns the error message.
let private readError (toml: string) =
    Assert.Throws<Exception>(fun () -> TomlConfigParser.read toml |> ignore).Message

[<Test>]
let ``Read: missing required key names the section and key``() =
    readError (general.Replace("namespace = \"SampleApp.AdventureWorks\"", ""))
    =! "[general] is missing required key 'namespace'."

[<Test>]
let ``Read: wrong value type names the key``() =
    readError (general.Replace("cli_mutable = true", "cli_mutable = \"yes\""))
    =! "[general] key 'cli_mutable' should be a Boolean, but is a String."

[<Test>]
let ``Read: a boolean where a string belongs is rejected``() =
    readError (general.Replace("connection = \"Data Source=localhost\"", "connection = true"))
    =! "[general] key 'connection' should be a String, but is a Boolean."

[<Test>]
let ``Read: a string where an array belongs is rejected``() =
    readError (general + "[filters]\ninclude = \"dbo/*\"")
    =! "[filters] key 'include' should be an Array, but is a String."

[<Test>]
let ``Read: a number in a restrictions array is rejected``() =
    readError (general + "[filters]\nrestrictions = { \"Tables\" = [ 1 ] }")
    =! "[filters.restrictions] key 'Tables' should be a String, but is an Integer."

[<Test>]
let ``Read: unknown keys and sections are ignored``() =
    let toml = general + "future_key = 1\n[future_section]\nkey = \"value\"\n[sqlhydra_query_integration]\nfuture_key = true"
    (TomlConfigParser.read toml).TableDeclarations =! true

[<Test>]
let ``Save then Read: round trips``() =
    let cfg =
        {
            ConnectionString = "Data Source=localhost"
            OutputFile = "AdventureWorks.fs"
            Namespace = "SampleApp.AdventureWorks"
            IsCLIMutable = true
            IsMutableProperties = false
            NullablePropertyType = NullablePropertyType.Option
            ProviderDbTypeAttributes = false
            TableDeclarations = true
            LeftJoinedViews = true
            Readers = Some { ReadersConfig.ReaderType = "Microsoft.Data.SqlClient.SqlDataReader" }
            Filters = { Filters.Empty with Includes = [ "dbo/*" ]; Excludes = [ "dbo/temp*" ] }
            TypeMappingExtensions = [ "SqlHydra.Extensions.NodaTime" ]
        }

    TomlConfigParser.read (TomlConfigParser.save cfg) =! cfg
