module SqlHydra.Extensions

open System
open System.IO
open System.Reflection
open System.Runtime.Loader
open SqlHydra.Domain
open Microsoft.Build.Construction

/// Filters extensions to only those matching a specific extension interface.
let ofType<'T when 'T :> ISqlHydraExtension> (extensions: ISqlHydraExtension list) : 'T list =
    extensions |> List.choose (function :? 'T as x -> Some x | _ -> None)

let private markerType = typeof<ISqlHydraExtension>

/// An AssemblyLoadContext that resolves shared dependencies (e.g. SqlHydra.Domain, FSharp.Core)
/// back to the host's already-loaded assemblies, ensuring interface type identity is preserved.
type private ExtensionLoadContext(pluginPath: string) =
    inherit AssemblyLoadContext(isCollectible = true)

    let resolver = AssemblyDependencyResolver(pluginPath)

    override this.Load(assemblyName: AssemblyName) =
        // First, check if the host already has this assembly loaded (e.g. SqlHydra.Domain, FSharp.Core).
        // This ensures the extension's IExtendTypeMapping is the same type as the host's.
        let hostAsm =
            AssemblyLoadContext.Default.Assemblies
            |> Seq.tryFind (fun a -> a.GetName().Name = assemblyName.Name)
        match hostAsm with
        | Some asm -> asm
        | None ->
            match resolver.ResolveAssemblyToPath(assemblyName) with
            | null -> null
            | path -> this.LoadFromAssemblyPath(path)

/// Discovers all ISqlHydraExtension implementations in the given assembly.
/// Uses ReflectionTypeLoadException fallback to handle types whose dependencies aren't available.
let private discoverExtensions (asm: Assembly) =
    let types =
        try
            asm.GetTypes()
        with
        | :? ReflectionTypeLoadException as ex ->
            ex.Types |> Array.filter (fun t -> t <> null)

    types
    |> Array.filter (fun t ->
        not t.IsAbstract && not t.IsInterface &&
        markerType.IsAssignableFrom(t))
    |> Array.map (fun t -> Activator.CreateInstance(t) :?> ISqlHydraExtension)
    |> Array.toList

/// Loads an assembly from a DLL path and discovers ISqlHydraExtension implementations.
let private loadFromAssembly (dllPath: string) =
    let fullPath = Path.GetFullPath(dllPath)
    let loadContext = ExtensionLoadContext(fullPath)
    let asm = loadContext.LoadFromAssemblyPath(fullPath)
    discoverExtensions asm

/// Finds a DLL by name in the project's bin/ directory.
let private findDll (project: FileInfo) (dllName: string) =
    let binDir = Path.Combine(project.Directory.FullName, "bin")
    if Directory.Exists(binDir) then
        Directory.EnumerateFiles(binDir, dllName, SearchOption.AllDirectories)
        |> Seq.tryHead
    else
        None

/// Auto-scans the target project's own assembly for ISqlHydraExtension implementations.
let scanProject (project: FileInfo) : ISqlHydraExtension list =
    let projectName = Path.GetFileNameWithoutExtension(project.Name)
    match findDll project $"{projectName}.dll" with
    | Some path -> loadFromAssembly path
    | None -> []

/// Loads named extension assemblies (from TOML [extensions] config).
/// Each name must be a PackageReference or ProjectReference in the target project.
let loadNamed (project: FileInfo) (extensionNames: string list) : ISqlHydraExtension list =
    extensionNames
    |> List.collect (fun extName ->
        let root = ProjectRootElement.Open(project.FullName)
        let hasRef =
            root.ItemGroups
            |> Seq.collect _.Items
            |> Seq.exists (fun item ->
                match item.ItemType with
                | "PackageReference" -> item.Include = extName
                | "ProjectReference" -> Path.GetFileNameWithoutExtension(item.Include) = extName
                | _ -> false
            )
        if not hasRef then
            failwith $"Extension '{extName}' was not found as a PackageReference or ProjectReference in '{project.Name}'."

        let dllName = $"{extName}.dll"
        match findDll project dllName with
        | None ->
            failwith $"Could not find '{dllName}' in the build output of '{project.Name}'. Ensure the project has been built."
        | Some path ->
            loadFromAssembly path
    )
