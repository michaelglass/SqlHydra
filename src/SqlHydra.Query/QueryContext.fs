namespace SqlHydra.Query

open System
open System.Data.Common
open System.Threading
open SqlKata
open SqlHydra.Domain

/// Indicates which execution strategy to use after command preparation.
type private InsertExecMode =
    | ExecNonQuery
    | ExecScalar
    | ExecOracleIdentity of DbParameter
    | ExecOutputClause of OutputField list

/// Contains methods that compile and read a query.
type QueryContext(conn: DbConnection, compiler: SqlKata.Compilers.Compiler) =
    
    let provider =
        match compiler with
        | :? SqlKata.Compilers.SqlServerCompiler -> SqlServer
        | :? SqlKata.Compilers.PostgresCompiler -> Npgsql
        | :? SqlKata.Compilers.MySqlCompiler -> MySql
        | :? SqlKata.Compilers.SqliteCompiler -> Sqlite
        | :? SqlKata.Compilers.OracleCompiler -> Oracle
        | _ -> failwith $"Unsupported compiler type: {compiler.GetType().FullName}"

    let setProviderDbType (param: DbParameter) (propertyName: string) (providerDbType: string) =
        let property = param.GetType().GetProperty(propertyName)
        let dbTypeSetter = property.GetSetMethod()
        let value = Enum.Parse(property.PropertyType, providerDbType)
        dbTypeSetter.Invoke(param, [|value|]) |> ignore
        
    let setParameterDbType (param: DbParameter) (qp: QueryParameter) =
        match provider, qp.ProviderDbType with
        | Npgsql, Some type' ->
            setProviderDbType param "NpgsqlDbType" type'
        | SqlServer, Some "SqlHierarchyId" ->
            param.GetType().GetProperty("UdtTypeName").SetValue(param, "hierarchyid") |> ignore
        | SqlServer, Some type' ->
            setProviderDbType param "SqlDbType" type'
        | _ -> ()

    /// Creates a DbParameter from a name and value, handling QueryParameter type and DateOnly/TimeOnly conversion.
    let createParam (cmd: DbCommand) (name: string) (value: obj) =
        let p = cmd.CreateParameter()
        p.ParameterName <- name
        p.Value <-
            match value with
            | :? QueryParameter as qp ->
                do setParameterDbType p qp
                qp.Value
            | _ -> value
            |> KataUtils.convertIfDateOnlyTimeOnly
        p :> Data.IDbDataParameter


    let mutable logger = fun (r: SqlResult) -> ()
        
    interface IDisposable with
        member this.Dispose() = 
            conn.Dispose()
            this.Transaction |> Option.iter (fun t -> t.Dispose())
            this.Transaction <- None
    
#if NETSTANDARD2_1_OR_GREATER
    interface IAsyncDisposable with
        member this.DisposeAsync() =
            task {
                do! conn.DisposeAsync()
                match this.Transaction with
                | Some t -> do! t.DisposeAsync()
                | None -> ()
                this.Transaction <- None
            } |> ValueTask
#endif
    
    member this.Connection = conn
    member this.Compiler = compiler
    member this.Provider = provider

    /// Logs a SqlKata compiled query with a user provided log function.
    /// Ex: queryContext.Logger <- printfn "SQL: %O"
    member this.Logger
        with get () = logger
        // Wrap the SqlResult to override query logging
        and set fn = logger <- LoggedSqlResult >> unbox<SqlResult> >> fn

    /// Updates the SqlResult with the latest cmd.CommandText and then logs the query.
    member private this.LogCommand (sqlResult: SqlResult, cmd: DbCommand) = 
        sqlResult.Sql <- cmd.CommandText
        this.Logger sqlResult

    member val Transaction : DbTransaction option = None with get,set

    member this.BeginTransaction(?isolationLevel: Data.IsolationLevel) =
        this.Transaction <- 
            match isolationLevel with
            | Some il -> conn.BeginTransaction(il) |> Some
            | None -> conn.BeginTransaction() |> Some

#if NETSTANDARD2_1_OR_GREATER
    // Return ValueTask to mirror DbConnection.BeginTransactionAsync, so that if F# ever gets a ValueTask CE we can use it here
    member this.BeginTransactionAsync(?isolationLevel: Data.IsolationLevel, ?cancellationToken: CancellationToken) = ValueTask <| task {
        let! trans =
            match isolationLevel with
            | Some il -> conn.BeginTransactionAsync(il, ?cancellationToken = cancellationToken)
            | None -> conn.BeginTransactionAsync(?cancellationToken = cancellationToken)
        this.Transaction <- Some trans
    }
#endif
    
    member this.CommitTransaction() =
        match this.Transaction with
        | Some t -> t.Commit(); this.Transaction <- None
        | None -> failwith "No transaction was started."
        
#if NETSTANDARD2_1_OR_GREATER
    member this.CommitTransactionAsync(?cancellationToken: CancellationToken) = task { 
        match this.Transaction with
        | Some t ->
            do! t.CommitAsync(?cancellationToken = cancellationToken)
            this.Transaction <- None
        | None -> failwith "No transaction was started."
    }
#endif

    member this.RollbackTransaction() =
        match this.Transaction with
        | Some t -> t.Rollback(); this.Transaction <- None
        | None -> failwith "No transaction was started."
        
#if NETSTANDARD2_1_OR_GREATER
    member this.RollbackTransactionAsync(?cancellationToken: CancellationToken) = task {
        match this.Transaction with
        | Some t ->
            do! t.RollbackAsync(?cancellationToken = cancellationToken)
            this.Transaction <- None
        | None -> failwith "No transaction was started."
    }
#endif

    member private this.TrySetTransaction(cmd: DbCommand) =
        this.Transaction |> Option.iter (fun t -> cmd.Transaction <- t)

    /// Builds a DbCommand with CommandText and Parameters from a SqlKata compiled query.
    member this.BuildCommand(compiledQuery: SqlResult, ?log: bool) = 
        let log = defaultArg log true
        if log then this.Logger compiledQuery
        let cmd = conn.CreateCommand()
        cmd |> this.TrySetTransaction
        cmd.CommandText <- compiledQuery.Sql
        for kvp in compiledQuery.NamedBindings do
            cmd.Parameters.Add(createParam cmd kvp.Key kvp.Value) |> ignore
        cmd

    /// Builds an ADO.NET DbCommand from a SqlKata query.
    member this.BuildCommand(query: Query) =
        let compiledQuery = compiler.Compile(query)
        // Apply PostgreSQL DISTINCT ON if present
        if provider = Npgsql then
            compiledQuery.Sql <- DistinctOnStore.applyToSql query compiledQuery.Sql
        this.BuildCommand(compiledQuery)

    /// Returns an ADO.NET data reader for a given query.
    member this.GetReader<'T, 'Reader & #DbDataReader> (query: SelectQuery<'T>) = 
        let cmd = this.BuildCommand(query.ToKataQuery()) // do not dispose cmd
        cmd.ExecuteReader() :?> 'Reader

    /// Returns an ADO.NET data reader for a given query.
    member this.GetReaderAsync<'T, 'Reader & #DbDataReader> (query: SelectQuery<'T>) = 
        this.GetReaderAsyncWithOptions<'T, 'Reader>(query)

    /// Returns an ADO.NET data reader for a given query.
    member this.GetReaderAsyncWithOptions<'T, 'Reader & #DbDataReader> (query: SelectQuery<'T>, ?cancel: CancellationToken) = 
        task { // Must wrap in task to prevent `EndExecuteNonQuery` ex in NET6_0_OR_GREATER
            let cmd = this.BuildCommand(query.ToKataQuery()) // do not dispose cmd
            let! reader = cmd.ExecuteReaderAsync(cancel |> Option.defaultValue CancellationToken.None)
            return reader :?> 'Reader
        }

    /// Executes a select query with a given readEntity builder function.
    [<System.Obsolete("This method will be removed in v4.0. Please use Select instead.")>]
    member this.Read<'Entity, 'Reader & #DbDataReader> (readEntityBuilder: 'Reader -> (unit -> 'Entity)) (query: SelectQuery<'Entity>) =
        this.Select<'Entity>(query)

    /// Executes a select query with a given readEntity builder function.
    [<System.Obsolete("This method will be removed in v4.0. Please use SelectAsync instead.")>]
    member this.ReadAsync<'Entity, 'Reader & #DbDataReader> (readEntityBuilder: 'Reader -> (unit -> 'Entity)) (query: SelectQuery<'Entity>) = 
        this.SelectAsync<'Entity>(query)

    /// Executes a select query with a given readEntity builder function and optional args.
    [<System.Obsolete("This method will be removed in v4.0. Please use SelectAsyncWithOptions instead.")>]
    member this.ReadAsyncWithOptions<'Entity, 'Reader & #DbDataReader>(query: SelectQuery<'Entity>, readEntityBuilder: 'Reader -> (unit -> 'Entity), ?cancel: CancellationToken) = 
        this.SelectAsyncWithOptions<'Entity>(query, ?cancel = cancel)

    /// Executes a select query with a given readEntity builder function for a single (option) result.
    [<System.Obsolete("This method will be removed in v4.0. Please use SelectOne instead.")>]
    member this.ReadOne<'Entity, 'Reader & #DbDataReader> (readEntityBuilder: 'Reader -> (unit -> 'Entity)) (query: SelectQuery<'Entity>) =
        this.SelectOne<'Entity>(query)

    /// Executes a select query with a given readEntity builder function for a single (option) result.
    [<System.Obsolete("This method will be removed in v4.0. Please use SelectOneAsync instead.")>]
    member this.ReadOneAsync<'Entity, 'Reader & #DbDataReader> (readEntityBuilder: 'Reader -> (unit -> 'Entity)) (query: SelectQuery<'Entity>) = 
        this.SelectOneAsync<'Entity>(query)

    /// Executes a select query with a given readEntity builder function for a single (option) result with optional args.
    [<System.Obsolete("This method will be removed in v4.0. Please use SelectOneAsyncWithOptions instead.")>]
    member this.ReadOneAsyncWithOptions<'Entity, 'Reader & #DbDataReader>(query: SelectQuery<'Entity>, readEntityBuilder: 'Reader -> (unit -> 'Entity), ?cancel: CancellationToken) = 
        this.SelectOneAsyncWithOptions<'Entity>(query, ?cancel = cancel)

    /// Executes a select query and returns results using the Hydration module.
    member this.Select<'Entity> (query: SelectQuery<'Entity>) =
        use cmd = this.BuildCommand(query.ToKataQuery())
        use reader = cmd.ExecuteReader()
        let readEntity = Hydration.buildRowReader<'Entity> this.Provider reader
        seq [|
            while reader.Read() do
                readEntity()
        |]

    /// Executes a select query asynchronously and returns results using the Hydration module.
    member this.SelectAsync<'Entity> (query: SelectQuery<'Entity>) =
        this.SelectAsyncWithOptions (query)

    /// Executes a select query asynchronously with optional args and returns results using the Hydration module.
    member this.SelectAsyncWithOptions<'Entity>(query: SelectQuery<'Entity>, ?cancel: CancellationToken) =
        task { // Must wrap in task to prevent `EndExecuteNonQuery` ex in NET6_0_OR_GREATER
            let cancel = defaultArg cancel CancellationToken.None
            use cmd = this.BuildCommand (query.ToKataQuery())
            use! reader = cmd.ExecuteReaderAsync(cancel)
            let readEntity = Hydration.buildRowReader<'Entity> this.Provider reader
            let results = ResizeArray<'Entity>()

            let! hasMore = reader.ReadAsync(cancel)
            let mutable hasMore = hasMore
            while hasMore && not cancel.IsCancellationRequested do
                results.Add(readEntity ())
                let! hasMore' = reader.ReadAsync(cancel)
                hasMore <- hasMore'

            return results :> seq<'Entity>
        }

    /// Executes a select query and returns a single (option) result using the Hydration module.
    member this.SelectOne<'Entity> (query: SelectQuery<'Entity>) =
        this.Select(query) |> Seq.tryHead

    /// Executes a select query asynchronously and returns a single (option) result using the Hydration module.
    member this.SelectOneAsync<'Entity> (query: SelectQuery<'Entity>) =
        this.SelectOneAsyncWithOptions(query)

    /// Executes a select query asynchronously for a single (option) result with optional args using the Hydration module.
    member this.SelectOneAsyncWithOptions<'Entity>(query: SelectQuery<'Entity>, ?cancel: CancellationToken) =
        task { // Must wrap in task to prevent `EndExecuteNonQuery` ex in NET6_0_OR_GREATER
            let! entities = this.SelectAsyncWithOptions (query, cancel |> Option.defaultValue CancellationToken.None)
            return entities |> Seq.tryHead
        }

    /// Prepares a DbCommand for an insert query, applying all SQL modifications (conflict handling, identity fixes, etc.)
    /// Returns the prepared command and an InsertExecMode indicating how to execute it.
    member private this.PrepareInsertCommand<'T, 'InsertReturn> (iq: InsertQuery<'T, 'InsertReturn>) =
        let compiledQuery = iq.ToKataQuery() |> compiler.Compile
        let cmd = this.BuildCommand(compiledQuery, log = false) // We will log manually below to capture query changes

        KataUtils.failIfIdentityOnConflict iq.Spec

        // Handle InsertOrUpdateOnUnique separately (SQL Server TRY/CATCH pattern)
        match iq.Spec.InsertType with
        | InsertOrUpdateOnUnique (keyFields, updateFields) ->
            let entity = iq.Spec.Entities |> List.head
            let propMap = FSharp.Reflection.FSharpType.GetRecordFields(typeof<'T>) |> Array.map (fun p -> p.Name, p) |> Map.ofArray
            let getColumnValue col = KataUtils.getQueryParameterForEntity entity propMap[col]
            let existingParams = [ for i in 0 .. cmd.Parameters.Count - 1 -> cmd.Parameters[i] :> Data.IDbDataParameter ]
            let newSql, allParams =
                InsertOrUpdateOnUnique.apply iq.Spec.Table keyFields updateFields cmd.CommandText existingParams (createParam cmd) getColumnValue
            cmd.CommandText <- newSql
            cmd.Parameters.Clear()
            for p in allParams do cmd.Parameters.Add(p) |> ignore
            this.LogCommand(compiledQuery, cmd)
            cmd, ExecNonQuery
        | _ ->

        // Applies on conflict modifier if in spec
        let applyOnConflict =
            match iq.Spec.InsertType with
            | InsertOrReplace -> OnConflict.insertOrReplace
            | OnConflictDoUpdate (conflictFields, updateFields) -> OnConflict.onConflictDoUpdate conflictFields updateFields
            | OnConflictDoUpdateCoalesce (conflictFields, updateFields, coalesceFields) -> OnConflict.onConflictDoUpdateCoalesce iq.Spec.Table conflictFields updateFields coalesceFields
            | OnConflictDoNothing conflictFields -> OnConflict.onConflictDoNothing conflictFields
            | OnConflictDoNothingWhereRaw (conflictFields, whereClause) -> OnConflict.onConflictDoNothingWhereRaw conflictFields whereClause
            | Insert -> id
            | InsertOrUpdateOnUnique _ -> id // handled above
            | InsertFromSelect _ -> id

        match iq.Spec with
        | { IdentityField = Some identityField } ->
            // Try apply on conflict
            cmd.CommandText <- cmd.CommandText |> applyOnConflict

            // Fix postgres identity
            if provider = Npgsql
            then cmd.CommandText <- cmd.CommandText |> Fixes.Postgres.fixIdentityQuery identityField

            // Fix oracle identity
            elif provider = Oracle
            then cmd.CommandText <- cmd.CommandText |> Fixes.Oracle.fixIdentityQuery identityField

            // Fix mssql guid identity
            elif provider = SqlServer && typeof<'InsertReturn> = typeof<System.Guid>
            then cmd.CommandText <- cmd.CommandText |> Fixes.MsSql.fixGuidIdentityQuery identityField

            this.LogCommand(compiledQuery, cmd)

            // Setup Oracle identity output parameter
            if provider = Oracle then
                let outputParam = cmd.CreateParameter()
                outputParam.ParameterName <- "outputParam"
                outputParam.DbType <- Data.DbType.Decimal
                outputParam.Direction <- Data.ParameterDirection.Output
                cmd.Parameters.Add(outputParam) |> ignore
                cmd, ExecOracleIdentity outputParam
            else
                cmd, ExecScalar

        | { OutputFields = outputFields } when outputFields.Length > 0 ->
            // Try apply on conflict
            cmd.CommandText <- cmd.CommandText |> applyOnConflict

            if provider = Npgsql then
                // Append PostgreSQL RETURNING clause
                let returningCsv =
                    outputFields
                    |> List.map (fun f -> $"\"{f.ColumnName}\"")
                    |> String.concat ", "
                cmd.CommandText <- cmd.CommandText.TrimEnd([|';'; ' '; '\n'; '\r'|]) + $" RETURNING {returningCsv};"
            else
                // Append SQL Server output clause
                cmd.CommandText <- OutputClause.inserted outputFields cmd.CommandText

            // Fix Oracle multi-insert query
            if provider = Oracle && iq.Spec.Entities.Length > 1
            then cmd.CommandText <- cmd.CommandText |> Fixes.Oracle.fixMultiInsertQuery

            this.LogCommand(compiledQuery, cmd)
            cmd, ExecOutputClause outputFields

        | _ ->
            // Try apply on conflict
            cmd.CommandText <- cmd.CommandText |> applyOnConflict

            // Fix Oracle multi-insert query
            if provider = Oracle && iq.Spec.Entities.Length > 1
            then cmd.CommandText <- cmd.CommandText |> Fixes.Oracle.fixMultiInsertQuery

            this.LogCommand(compiledQuery, cmd)
            cmd, ExecNonQuery

    member this.Insert<'T, 'InsertReturn> (iq: InsertQuery<'T, 'InsertReturn>) =
        let cmd, execMode = this.PrepareInsertCommand(iq)
        use cmd = cmd
        match execMode with
        | ExecNonQuery ->
            let results = cmd.ExecuteNonQuery()
            Convert.ChangeType(results, typeof<'InsertReturn>) :?> 'InsertReturn
        | ExecScalar ->
            let identity = cmd.ExecuteScalar()
            Convert.ChangeType(identity, typeof<'InsertReturn>) :?> 'InsertReturn
        | ExecOracleIdentity outputParam ->
            let _ = cmd.ExecuteNonQuery()
            Convert.ChangeType(outputParam.Value, typeof<'InsertReturn>) :?> 'InsertReturn
        | ExecOutputClause outputFields ->
            OutputClause.readValues<'InsertReturn> cmd CancellationToken.None outputFields
            |> Async.AwaitTask |> Async.RunSynchronously

    member this.InsertAsync<'T, 'InsertReturn> (query: InsertQuery<'T, 'InsertReturn>) =
        this.InsertAsyncWithOptions(query)

    member this.InsertAsyncWithOptions<'T, 'InsertReturn> (iq: InsertQuery<'T, 'InsertReturn>, ?cancel: CancellationToken) =
        task { // Must wrap in task to prevent `EndExecuteNonQuery` ex in NET6_0_OR_GREATER
            let cmd, execMode = this.PrepareInsertCommand(iq)
            use cmd = cmd
            let cancel = defaultArg cancel CancellationToken.None
            match execMode with
            | ExecNonQuery ->
                let! results = cmd.ExecuteNonQueryAsync(cancel)
                return Convert.ChangeType(results, typeof<'InsertReturn>) :?> 'InsertReturn
            | ExecScalar ->
                let! identity = cmd.ExecuteScalarAsync(cancel)
                return Convert.ChangeType(identity, typeof<'InsertReturn>) :?> 'InsertReturn
            | ExecOracleIdentity outputParam ->
                let! _ = cmd.ExecuteNonQueryAsync(cancel)
                return Convert.ChangeType(outputParam.Value, typeof<'InsertReturn>) :?> 'InsertReturn
            | ExecOutputClause outputFields ->
                let! outputValues = OutputClause.readValues<'InsertReturn> cmd cancel outputFields
                return outputValues
        }
    
    /// Adds raw SET parameter bindings from SetRawParamStore to a command.
    member private this.ApplySetRawParams(cmd: DbCommand, kataQuery: Query) =
        match SetRawParamStore.tryGet kataQuery with
        | Some parms ->
            for (name, value) in parms do
                cmd.Parameters.Add(createParam cmd name value) |> ignore
        | None -> ()

    member this.Update (query: UpdateQuery<'T, 'UpdateReturn>) =
        let kataQuery = query.ToKataQuery()
        use cmd = this.BuildCommand(kataQuery)
        this.ApplySetRawParams(cmd, kataQuery)
        cmd.ExecuteNonQuery()

    member this.UpdateAsync (query: UpdateQuery<'T, 'UpdateReturn>) =
        this.UpdateAsyncWithOptions(query)

    member this.UpdateAsyncWithOptions (query: UpdateQuery<'T, 'UpdateReturn>, ?cancel: CancellationToken) =
        task { // Must wrap in task to prevent `EndExecuteNonQuery` ex in NET6_0_OR_GREATER
            let cancel = defaultArg cancel CancellationToken.None

            let kataQuery = query.ToKataQuery()
            use cmd = this.BuildCommand(kataQuery)
            this.ApplySetRawParams(cmd, kataQuery)

            if query.Spec.OutputFields.Length > 0 then
                // Append SQL Server output clause
                cmd.CommandText <- OutputClause.updated query.Spec.OutputFields cmd.CommandText
                let! outputValues = OutputClause.readValues<'UpdateReturn> cmd cancel query.Spec.OutputFields
                return outputValues
            else
                let! rowsInserted = cmd.ExecuteNonQueryAsync(cancel)
                return Convert.ChangeType(rowsInserted, typeof<'UpdateReturn>) :?> 'UpdateReturn
        }

    member this.Delete (query: DeleteQuery<'T>) = 
        use cmd = this.BuildCommand(query.ToKataQuery())
        cmd.ExecuteNonQuery()

    member this.DeleteAsync (query: DeleteQuery<'T>) = 
        this.DeleteAsyncWithOptions(query)

    member this.DeleteAsyncWithOptions (query: DeleteQuery<'T>, ?cancel: CancellationToken) = 
        task { // Must wrap in task to prevent `EndExecuteNonQuery` ex in NET6_0_OR_GREATER
            use cmd = this.BuildCommand(query.ToKataQuery())
            return! cmd.ExecuteNonQueryAsync(cancel |> Option.defaultValue CancellationToken.None)
        }

    member this.Count (query: SelectQuery<int>) = 
        use cmd = this.BuildCommand(query.ToKataQuery())
        match cmd.ExecuteScalar() with
        | :? int64 as count -> Convert.ToInt32 count
        | _  as count -> count :?> int

    member this.CountAsync (query: SelectQuery<int>) = 
        this.CountAsyncWithOptions(query)

    member this.CountAsyncWithOptions (query: SelectQuery<int>, ?cancel: CancellationToken) = 
        task { // Must wrap in task to prevent `EndExecuteNonQuery` ex in NET6_0_OR_GREATER
            use cmd = this.BuildCommand(query.ToKataQuery())
            match! cmd.ExecuteScalarAsync(cancel |> Option.defaultValue CancellationToken.None) with
            | :? int64 as count -> return Convert.ToInt32 count
            | count -> return count :?> int
        }