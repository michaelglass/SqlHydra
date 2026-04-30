/// Linq delete query builders
[<AutoOpen>]
module SqlHydra.Query.DeleteBuilders

open System
open System.Threading

let private prepareDeleteQuery<'Deleted> (ir: SelectQueryIR) =
    DeleteQuery<'Deleted>({ Table = ir.From |> Option.defaultValue ""; Where = ir.Where; Returning = ir.Returning })

/// The base delete builder that contains all common operations
type DeleteBuilder<'Deleted>() =

    let getQueryOrDefault (state: QuerySource<'T>) =
        match state with
        | :? QuerySource<'T, SelectQueryIR> as qs -> qs.Query
        | _ -> SelectQueryIR.empty

    member val CancellationToken = CancellationToken.None with get, set

    member this.For (state: QuerySource<'T>, [<ReflectedDefinition>] forExpr: FSharp.Quotations.Expr<'T -> QuerySource<'T>>) =
        let ir = state |> getQueryOrDefault
        let tableAlias = QuotationVisitor.visitFor forExpr |> QuotationVisitor.allowUnderscore true
        let tblMaybe, tableMappings = TableMappings.tryGetByRootOrAlias tableAlias state.TableMappings
        let tbl = tblMaybe |> Option.get

        QuerySource<'T, SelectQueryIR>(
            { ir with From = Some $"{tbl.Schema}.{tbl.Name}" },
            tableMappings)

    member this.Yield _ =
        QuerySource<'T>(Map.empty)

    /// Sets the WHERE condition
    [<CustomOperation("where", MaintainsVariableSpace = true)>]
    member this.Where (state:QuerySource<'T>, [<ProjectionParameter>] whereExpression) =
        let ir = state |> getQueryOrDefault
        let tableMappings = state.TableMappings |> Map.values
        let newClause = LinqExpressionVisitors.visitWhere<'T> tableMappings whereExpression (FQ.fullyQualifyColumn state.TableMappings)
        QuerySource<'T, SelectQueryIR>({ ir with Where = WhereClause.combineAnd ir.Where newClause }, state.TableMappings)

    /// Adds a column to the DELETE ... RETURNING clause (PostgreSQL).
    [<CustomOperation("returning", MaintainsVariableSpace = true)>]
    member this.Returning (state: QuerySource<'T>, [<ProjectionParameter>] propertySelector: System.Linq.Expressions.Expression<Func<'T, 'Prop>>) =
        let ir = state |> getQueryOrDefault
        let prop = LinqExpressionVisitors.visitPropertySelector<'T, 'Prop> propertySelector :?> Reflection.PropertyInfo
        QuerySource<'T, SelectQueryIR>({ ir with Returning = ir.Returning @ [ prop.Name ] }, state.TableMappings)

    /// Deletes all records in the table (only when there are is no where clause)
    [<CustomOperation("deleteAll", MaintainsVariableSpace = true)>]
    member this.DeleteAll (state:QuerySource<'T>) =
        state :?> QuerySource<'T, SelectQueryIR>

    /// Sets a CancellationToken for the query execution.
    [<CustomOperation("cancel", MaintainsVariableSpace = true)>]
    member this.Cancel (state: QuerySource<'T, SelectQueryIR>, cancellationToken: CancellationToken) =
        this.CancellationToken <- cancellationToken
        state

    /// Unwraps the query
    member this.Run (state: QuerySource<'Deleted>) =
        state
        |> getQueryOrDefault
        |> prepareDeleteQuery


/// A delete builder that returns an Async result.
type DeleteAsyncBuilder<'Deleted>(ct: ContextType) =
    inherit DeleteBuilder<'Deleted>()

    member this.Run (state: QuerySource<'Deleted, SelectQueryIR>) =
        async {
            let deleteQuery = state.Query |> prepareDeleteQuery
            let! ctx = ContextUtils.getContext ct |> Async.AwaitTask
            try
                let! asyncCancel = Async.CancellationToken
                let cancel = if this.CancellationToken <> CancellationToken.None then this.CancellationToken else asyncCancel
                let! result = ctx.DeleteAsyncWithOptions (deleteQuery, cancel) |> Async.AwaitTask
                return result
            finally
                ContextUtils.disposeIfNotShared ct ctx
        }


/// A delete builder that returns a Task result.
type DeleteTaskBuilder<'Deleted>(ct: ContextType) =
    inherit DeleteBuilder<'Deleted>()

    member this.Run (state: QuerySource<'Deleted, SelectQueryIR>) =
        task {
            let deleteQuery = state.Query |> prepareDeleteQuery
            let! ctx = ContextUtils.getContext ct
            try
                let! result = ctx.DeleteAsyncWithOptions (deleteQuery, this.CancellationToken) |> Async.AwaitTask
                return result
            finally
                ContextUtils.disposeIfNotShared ct ctx
        }

/// Builds and returns a delete query that can be manually run by piping into QueryContext delete methods
let delete<'Deleted> =
    DeleteBuilder<'Deleted>()

/// Builds and returns a delete query that returns an Async result
let inline deleteAsync< ^Deleted, ^Context
    when (ContextTypeResolver.Resolver or ^Context) : (static member ($) : ContextTypeResolver.Resolver * ^Context -> ContextType)>
    (ctSource: ^Context) =
    let ct = ContextTypeResolver.resolve ctSource
    DeleteAsyncBuilder< ^Deleted>(ct)

/// Builds and returns a delete query that returns a Task result
let inline deleteTask< ^Deleted, ^Context
    when (ContextTypeResolver.Resolver or ^Context) : (static member ($) : ContextTypeResolver.Resolver * ^Context -> ContextType)>
    (ctSource: ^Context) =
    let ct = ContextTypeResolver.resolve ctSource
    DeleteTaskBuilder< ^Deleted>(ct)

