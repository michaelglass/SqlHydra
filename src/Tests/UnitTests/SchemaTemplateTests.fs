module UnitTests.SchemaTemplateTests

open System
open NUnit.Framework
open Swensen.Unquote
open SqlHydra
open SqlHydra.Domain

let private baseCfg () : Config =
    {
        ConnectionString = "Host=localhost;Database=testdb"
        OutputFile = "Test.fs"
        Namespace = "TestNS"
        IsCLIMutable = true
        IsMutableProperties = false
        NullablePropertyType = NullablePropertyType.Option
        ProviderDbTypeAttributes = false
        TableDeclarations = false
        Readers = None
        Filters = Filters.Empty
        TypeMappingExtensions = []
    }

let private version () : Version.InformationalVersion =
    {
        InformationalVersion = "4.1.0-beta.2"
        Version = Version(4, 1, 0)
        PreReleaseSuffix = Some "beta.2"
    }

let private moodEnum () : Enum =
    {
        Schema = "public"
        Name = "mood"
        Labels =
            [
                { Name = "happy"; SortOrder = 0 }
                { Name = "ok"; SortOrder = 1 }
                { Name = "sad"; SortOrder = 2 }
            ]
    }

let private schemaWithEnums () : Schema = { Tables = []; Enums = [ moodEnum () ] }
let private schemaNoEnums () : Schema = { Tables = []; Enums = [] }

[<Test>]
let ``Npgsql RenderEnumRegistration produces register helper for a public enum`` () =
    match Npgsql.Provider.instance.RenderEnumRegistration(baseCfg (), schemaWithEnums ()) with
    | Some s ->
        test <@ s.Contains "let register (builder: Npgsql.NpgsqlDataSourceBuilder)" @>
        test <@ s.Contains "MapEnum<``public``.mood>(\"mood\")" @>
    | None ->
        Assert.Fail "Expected Some registration fragment for Npgsql with one enum"

[<Test>]
let ``SchemaTemplate generate emits module Enums for Npgsql with enums`` () =
    let output =
        SchemaTemplate.generate (baseCfg ()) Npgsql.Provider.instance (schemaWithEnums ()) (version ()) []

    test <@ output.Contains "module Enums" @>
    test <@ output.Contains "MapEnum<``public``.mood>(\"mood\")" @>

[<Test>]
let ``SqlServer RenderEnumRegistration is None even when enums are present`` () =
    let result = SqlServer.Provider.instance.RenderEnumRegistration(baseCfg (), schemaWithEnums ())
    test <@ result = None @>

[<Test>]
let ``SchemaTemplate generate emits no module Enums for an empty-enum schema`` () =
    let output =
        SchemaTemplate.generate (baseCfg ()) SqlServer.Provider.instance (schemaNoEnums ()) (version ()) []

    test <@ not (output.Contains "module Enums") @>
