module UnitTests.ContributeColumns

open System
open NUnit.Framework
open Swensen.Unquote
open SqlHydra
open SqlHydra.Domain
open SqlHydra.Query

// ---------------------------------------------------------------------------------------
// Fixtures: a discovered schema, and extensions that contribute to it.
// ---------------------------------------------------------------------------------------

let private mapping clrType dbType providerDbType alias =
    {
        TypeMapping.ClrType = clrType
        TypeMapping.DbType = dbType
        TypeMapping.ProviderDbType = providerDbType
        TypeMapping.ColumnTypeAlias = alias
    }

let private discovered name =
    {
        Column.Name = name
        Column.TypeMapping = mapping "int" Data.DbType.Int32 None "int4"
        Column.IsNullable = false
        Column.IsPK = name = "id"
        Column.IsReadOnly = false
        Column.Doc = []
    }

let private mkTable schema name tableType columns =
    {
        Table.Catalog = ""
        Table.Schema = schema
        Table.Name = name
        Table.Type = tableType
        Table.Columns = columns
        Table.TotalColumns = List.length columns
    }

let private usersTable = mkTable "public" "users" TableType.Table [ discovered "id"; discovered "age" ]
let private activeUsersView = mkTable "public" "active_users" TableType.View [ discovered "id" ]

let private schema: Schema =
    {
        Tables = [ usersTable; activeUsersView ]
        Enums = []
    }

/// The column PgSystemColumns needs and cannot currently produce: not in `information_schema`,
/// so no type mapping is ever consulted for it; `uint32` with no `NpgsqlDbType` throws
/// client-side on a compare-and-swap; and the database owns the value.
let private xminColumn =
    {
        Column.Name = "xmin"
        Column.TypeMapping = mapping "uint" Data.DbType.UInt32 (Some "Xid") "xid"
        Column.IsNullable = false
        Column.IsPK = false
        Column.IsReadOnly = true
        Column.Doc =
            [ "The id of the transaction that inserted this row version — PostgreSQL's row version."
              "It changes on every write to the row." ]
    }

/// Contributes `xmin` to PostgreSQL base tables only — a view has no system columns.
type XminContribution() =
    interface IContributeColumns with
        member _.Contribute(baseFn) =
            fun (ctx: ColumnContributionContext) ->
                let contributed = baseFn ctx

                if ctx.Provider = ProviderType.Npgsql && ctx.Table.Type = TableType.Table then
                    contributed @ [ xminColumn ]
                else
                    contributed

/// A second extension, to pin down composition order and that each sees the running list.
type CtidContribution() =
    interface IContributeColumns with
        member _.Contribute(baseFn) =
            fun ctx ->
                baseFn ctx
                @ [ { xminColumn with
                        Name = "ctid"
                        TypeMapping = mapping "NpgsqlTypes.NpgsqlTid" Data.DbType.Object (Some "Tid") "tid" } ]

let private apply extensions = Extensions.contributeColumns extensions ProviderType.Npgsql schema

let private columnNames tableName (s: Schema) =
    s.Tables |> List.find (fun t -> t.Name = tableName) |> _.Columns |> List.map _.Name

// ---------------------------------------------------------------------------------------
// The seam itself
// ---------------------------------------------------------------------------------------

[<Test>]
let ``Contributes a column the provider could not discover`` () =
    let result = apply [ XminContribution() ]

    test <@ columnNames "users" result = [ "id"; "age"; "xmin" ] @>

[<Test>]
let ``Contributed column keeps the type mapping the extension gave it`` () =
    let result = apply [ XminContribution() ]
    let xmin = result.Tables |> List.find (fun t -> t.Name = "users") |> _.Columns |> List.last

    test <@ xmin.TypeMapping.ClrType = "uint" @>
    test <@ xmin.TypeMapping.ProviderDbType = Some "Xid" @>
    test <@ xmin.IsReadOnly @>

[<Test>]
let ``Context carries the table, so an extension can skip a view`` () =
    let result = apply [ XminContribution() ]

    // A view has no system columns; the extension decided that, not the seam.
    test <@ columnNames "active_users" result = [ "id" ] @>

[<Test>]
let ``Context carries the provider, so a column contributes only where it exists`` () =
    let result = Extensions.contributeColumns [ XminContribution() ] ProviderType.Sqlite schema

    test <@ columnNames "users" result = [ "id"; "age" ] @>

[<Test>]
let ``Extensions compose in registration order, each wrapping the last`` () =
    let result = apply [ XminContribution(); CtidContribution() ]

    test <@ columnNames "users" result = [ "id"; "age"; "xmin"; "ctid" ] @>

[<Test>]
let ``No extensions leaves the schema untouched`` () =
    test <@ Extensions.contributeColumns [] ProviderType.Npgsql schema = schema @>

[<Test>]
let ``Contributing a discovered column's name raises rather than shadowing it`` () =
    let collide =
        { new IContributeColumns with
            member _.Contribute(baseFn) = fun ctx -> baseFn ctx @ [ discovered "age" ] }

    let ex = Assert.Throws<Exception>(fun () -> apply [ collide ] |> ignore)

    test <@ ex.Message.Contains "age" @>
    test <@ ex.Message.Contains "public.users" @>

[<Test>]
let ``Two extensions contributing the same name raises`` () =
    let ex = Assert.Throws<Exception>(fun () -> apply [ XminContribution(); XminContribution() ] |> ignore)

    test <@ ex.Message.Contains "xmin" @>
    test <@ ex.Message.Contains "public.users" @>

// ---------------------------------------------------------------------------------------
// What the contributed column becomes in the generated file
// ---------------------------------------------------------------------------------------

let private cfg: Config =
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

let private version: Version.InformationalVersion =
    {
        InformationalVersion = "0.0.0"
        Version = Version(0, 0, 0)
        PreReleaseSuffix = None
    }

let private generate namingExts s =
    SchemaTemplate.generate cfg SqlHydra.Npgsql.Provider.instance s version namingExts

[<Test>]
let ``Generated field carries the provider db type the extension asked for`` () =
    let code = apply [ XminContribution() ] |> generate []

    // Mandatory, not decoration: Npgsql has no default mapping for uint32, so a parameter
    // without it throws client-side.
    test <@ code.Contains "[<ProviderDbType(\"Xid\")>]" @>
    test <@ code.Contains "xmin: uint" @>

[<Test>]
let ``Generated field carries ReadOnlyColumn when the extension marks it`` () =
    let code = apply [ XminContribution() ] |> generate []

    test <@ code.Contains "[<ReadOnlyColumn>]" @>

[<Test>]
let ``A contributed column carries the doc comment the extension gave it`` () =
    let code = apply [ XminContribution() ] |> generate []

    // The caution belongs on the field, not only in the extension's README: whoever reaches
    // for the column is reading the generated type, not the extension's docs.
    test <@ code.Contains "/// It changes on every write to the row." @>

[<Test>]
let ``Discovered columns are not marked read-only`` () =
    let code = generate [] schema

    test <@ not (code.Contains "[<ReadOnlyColumn>]") @>

[<Test>]
let ``A naming extension renames a contributed column like any other`` () =
    let upperCase =
        { new IExtendNaming with
            member _.ExtendTableName(baseFn) = baseFn
            member _.ExtendColumnName(baseFn) = fun ctx -> (baseFn ctx).ToUpper() }

    let code = apply [ XminContribution() ] |> generate [ upperCase ]

    test <@ code.Contains "XMIN: uint" @>
    test <@ not (code.Contains "xmin: uint") @>

// ---------------------------------------------------------------------------------------
// What `[<ReadOnlyColumn>]` means at query time
// ---------------------------------------------------------------------------------------

[<CLIMutable>]
type widget =
    {
        id: int
        name: string

        [<ReadOnlyColumn>]
        [<ProviderDbType("Xid")>]
        xmin: uint32
    }

[<CLIMutable>]
type plain = { id: int; name: string }

let private widgets = table<widget>
let private plains = table<plain>
let private row = { id = 1; name = "a"; xmin = 42u }

[<Test>]
let ``An insert omits a read-only column`` () =
    let ir = QueryUtils.fromInsert (insert { for w in widgets do entity row }).Spec

    test <@ ir.Columns = [ "id"; "name" ] @>

[<Test>]
let ``An insert of a record with no read-only column is unaffected`` () =
    let ir = QueryUtils.fromInsert (insert { for p in plains do entity { id = 1; name = "a" } }).Spec

    test <@ ir.Columns = [ "id"; "name" ] @>

[<Test>]
let ``An entity update omits a read-only column`` () =
    // The point of dropping rather than rejecting: `entity` still works with a row that was
    // read back carrying a real version.
    let ir =
        QueryUtils.fromUpdate
            (update {
                for w in widgets do
                    entity row
                    where (w.id = 1)
             })
                .Spec

    test <@ ir.SetColumns |> List.map fst = [ "id"; "name" ] @>

[<Test>]
let ``An explicit set of a read-only column is dropped`` () =
    // PostgreSQL refuses `SET xmin = ...` outright, so passing it through only turns a
    // compile-time mistake into a runtime one.
    let ir =
        QueryUtils.fromUpdate
            (update {
                for w in widgets do
                    set w.name "b"
                    set w.xmin 7u
                    where (w.id = 1)
             })
                .Spec

    test <@ ir.SetColumns |> List.map fst = [ "name" ] @>
