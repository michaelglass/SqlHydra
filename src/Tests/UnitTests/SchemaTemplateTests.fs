module UnitTests.SchemaTemplateTests

open NUnit.Framework
open Swensen.Unquote
open SqlHydra.Domain
open SqlHydra

// A minimal Config suitable for exercising SchemaTemplate.generate without a live database.
let private mkCfg () : Config =
    {
        ConnectionString = ""
        OutputFile = ""
        Namespace = "TestNS"
        IsCLIMutable = true
        IsMutableProperties = false
        NullablePropertyType = NullablePropertyType.Option
        ProviderDbTypeAttributes = true
        TableDeclarations = false
        Readers = None
        Filters = Filters.Empty
        TypeMappingExtensions = []
    }

let private mkVersion () : Version.InformationalVersion =
    {
        InformationalVersion = "0.0.0"
        Version = System.Version(0, 0, 0)
        PreReleaseSuffix = None
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

let private generateWith (cfg: Config) (db: Schema) =
    SchemaTemplate.generate cfg SqlHydra.Npgsql.Provider.instance db (mkVersion ()) []

let private generate (db: Schema) = generateWith (mkCfg ()) db

[<Test>]
let ``Generates Enums registration module for Npgsql enum`` () =
    let db: Schema = { Tables = []; Enums = [ moodEnum () ] }
    let output = generate db

    test <@ output.Contains "module Enums" @>
    test <@ output.Contains "let register (builder: Npgsql.NpgsqlDataSourceBuilder)" @>
    test <@ output.Contains "MapEnum<``public``.mood>(\"mood\")" @>

[<Test>]
let ``Does not generate Enums registration module when no enums`` () =
    let db: Schema = { Tables = []; Enums = [] }
    let output = generate db

    test <@ not (output.Contains "module Enums") @>

[<Test>]
let ``Does not generate Enums registration module when ProviderDbTypeAttributes is off`` () =
    let cfg = { mkCfg () with ProviderDbTypeAttributes = false }
    let db: Schema = { Tables = []; Enums = [ moodEnum () ] }
    let output = generateWith cfg db

    // Without ProviderDbTypeAttributes the generated file must not require an Npgsql package reference.
    test <@ not (output.Contains "module Enums") @>
    test <@ not (output.Contains "Npgsql.NpgsqlDataSourceBuilder") @>

[<Test>]
let ``Factory registers enums when enums exist`` () =
    let db: Schema = { Tables = []; Enums = [ moodEnum () ] }
    let output = generate db

    test <@ output.Contains "(Npgsql.NpgsqlDataSourceBuilder(connectionString) |> Enums.register).Build()" @>

[<Test>]
let ``Factory uses plain data source when no enums`` () =
    let db: Schema = { Tables = []; Enums = [] }
    let output = generate db

    test <@ output.Contains "Npgsql.NpgsqlDataSource.Create(connectionString)" @>
    test <@ not (output.Contains "Enums.register") @>

// --- Spike: write record emission -----------------------------------------------------------

let private column (name: string) (clrType: string) (isReadOnly: bool) : Column =
    {
        Name = name
        TypeMapping =
            { ClrType = clrType; DbType = System.Data.DbType.Object; ProviderDbType = None; ColumnTypeAlias = clrType }
        IsNullable = false
        IsPK = false
        IsReadOnly = isReadOnly
    }

let private tableOf (name: string) (columns: Column list) : Table =
    { Catalog = ""; Schema = "sales"; Name = name; Type = TableType.Table; Columns = columns; TotalColumns = columns.Length }

let private generateTable (table: Table) =
    generate { Tables = [ table ]; Enums = [] }

let private generatedWriteRecordFor (name: string) (output: string) =
    output.Contains $"type {name}_write ="

[<Test>]
let ``Spike: a table with read-only columns gets a write record holding the other columns`` () =
    let invoice =
        tableOf "invoice"
            [ column "id" "int" true
              column "price" "decimal" false
              column "code" "string" false
              column "tax" "decimal" true ]
    let output = generateTable invoice
    printfn "%s" output

    test <@ generatedWriteRecordFor "invoice" output @>
    test <@ output.Contains "interface IWriteOf<invoice>" @>
    // The write record names only the writable columns.
    let writeRecord = output.Substring(output.IndexOf "type invoice_write =")
    test <@ writeRecord.Contains "price: decimal" @>
    test <@ writeRecord.Contains "code: string" @>
    test <@ not (writeRecord.Contains "id: int") @>
    test <@ not (writeRecord.Contains "tax: decimal") @>
    test <@ not (writeRecord.Contains "ReadOnlyColumn") @>

[<Test>]
let ``Spike: a table with no read-only columns gets no write record`` () =
    let plain = tableOf "plain" [ column "id" "int" false; column "name" "string" false ]
    let output = generateTable plain
    printfn "%s" output

    test <@ output.Contains "type plain =" @>
    test <@ not (generatedWriteRecordFor "plain" output) @>
    test <@ not (output.Contains "IWriteOf") @>

[<Test>]
let ``Spike: a relation where every column is read-only gets no write record (open question)`` () =
    let allGenerated = tableOf "all_generated" [ column "id" "int" true; column "stamp" "System.DateTime" true ]
    let output = generateTable allGenerated
    printfn "%s" output

    test <@ output.Contains "type all_generated =" @>
    test <@ not (generatedWriteRecordFor "all_generated" output) @>
