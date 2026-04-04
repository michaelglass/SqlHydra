module SqlHydra.Domain

open System.Data

let private valueTypes = 
    Set [ "bool"; "int"; "int64"; "int16"; "byte"; "decimal"; "double"; "System.Single"
          "System.DateTimeOffset"; "System.DateTime"; "System.DateOnly"; "System.TimeOnly"
          "System.Guid"; ]

let isValueType (typeName: string) = 
    valueTypes.Contains typeName

type TypeMapping =
    {
        ClrType: string
        DbType: DbType
        ProviderDbType: string option
        ColumnTypeAlias: string
    }
    member this.IsValueType() = 
        isValueType this.ClrType

type Column = 
    {
        Name: string
        TypeMapping: TypeMapping
        IsNullable: bool
        IsPK: bool
    }

type TableType = 
    | Table = 0
    | View = 1

type Table = 
    {
        Catalog: string
        Schema: string
        Name: string
        Type: TableType
        Columns: Column list
        TotalColumns: int
    }

type EnumLabel = 
    {
        Name: string
        SortOrder: int
    }

type Enum = 
    {
        Name: string
        Schema: string
        Labels: EnumLabel list
    }

type Schema =
    {
        Tables: Table list

        /// Support for Postgres enums
        Enums: Enum list
    }

type ReadersConfig = 
    {
        /// A fully qualified reader type. Ex: "Microsoft.Data.SqlClient.SqlDataReader"
        ReaderType: string
    }

type Filters = 
    {
        /// Glob patterns to include "{schema}/{table}.{column}"
        Includes: string list
        /// Glob patterns to exclude "{schema}/{table}.{column}"
        Excludes: string list        
        /// Restrictions applied to GetSchema() calls. Ex: Map [ "Tables", [| "dbo" |]; "Views", [||]; "Columns", [||] ]
        Restrictions: Map<string, string array>
    }
    static member Empty = { Includes = []; Excludes = []; Restrictions = Map.empty }
    member this.TryGetRestrictionsByKey (key: string) = 
        this.Restrictions.TryFind key 
        |> Option.defaultValue [||]        

type Config = 
    {
        /// General: Db conneciton string
        ConnectionString: string

        /// General: path to the generated .fs output file
        OutputFile: string

        /// General: namespace for the generated .fs output file
        Namespace: string

        /// General: if true, makes generated table records CLIMutable
        IsCLIMutable: bool

        /// General: if true, makes generated table record properties mutable
        IsMutableProperties: bool

        /// General: determines whether to use F# Option or System.Nullable for nullable columns.
        NullablePropertyType: NullablePropertyType
        
        /// SqlHydra.Query Integration: generates support for creating Db specific parameter types
        ProviderDbTypeAttributes: bool
        
        /// SqlHydra.Query Integration: creates a SqlHydra.Query table declaration for each table
        TableDeclarations: bool

        /// Readers: provides a Db provider specific IDataReader type (for access to Db-specific features)
        Readers: ReadersConfig option
        
        /// Filters: optional filters for schemas, tables and columns
        Filters: Filters

        /// Extensions: type mapping extension assembly names
        TypeMappingExtensions: string list
    }

and [<RequireQualifiedAccess>] 
    NullablePropertyType = 
    | Option
    | Nullable

type ProviderType =
    | SqlServer
    | Npgsql
    | Sqlite
    | MySql
    | Oracle
    | Custom of string

type ColumnSchema =
    {
        Catalog: string
        Schema: string
        Table: string
        Name: string
        ProviderTypeName: string
        IsNullable: bool
        Ordinal: int
        Precision: int option
        Scale: int option
        IsPrimaryKey: bool
        IsComputed: bool
        DefaultValue: string option
    }

type TableSchema =
    {
        Catalog: string
        Schema: string
        Name: string
        Type: TableType
        Columns: ColumnSchema list
    }

type ISqlHydraExtension = interface end

type TypeMappingContext =
    {
        Table: TableSchema
        Column: ColumnSchema
    }

type IExtendTypeMapping =
    inherit ISqlHydraExtension
    abstract member Extend: baseTryFind: (TypeMappingContext -> TypeMapping option) -> (TypeMappingContext -> TypeMapping option)

type NamingContext =
    {
        Table: Table
        Column: Column option
    }

type IExtendNaming =
    inherit ISqlHydraExtension
    abstract member ExtendTableName: baseFn: (NamingContext -> string) -> (NamingContext -> string)
    abstract member ExtendColumnName: baseFn: (NamingContext -> string) -> (NamingContext -> string)

type ISqlHydraDbProvider =
    abstract member Id: string
    abstract member Name: string
    abstract member Type: ProviderType
    abstract member DefaultReaderType: string
    abstract member DefaultProvider: string
    abstract member SqlKataCompiler: string
    abstract member ProviderConnectionType: string
    abstract member GetSchema: cfg: Config * isLegacy: bool * extensions: IExtendTypeMapping list -> Schema
