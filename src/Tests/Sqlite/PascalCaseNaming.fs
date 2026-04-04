namespace Sqlite.CustomNaming

open SqlHydra.Domain

///// Sample naming extension that renames specific columns.
//type PascalCaseNaming() =
//    interface IExtendNaming with
//        member _.ExtendTableName(baseFn) =
//            fun ctx ->
//                let name = baseFn ctx
//                name

//        member _.ExtendColumnName(baseFn) =
//            fun ctx ->
//                let name = baseFn ctx
//                // Example: convert rowguid -> RowGuid
//                if name = "rowguid" then "RowGuid" else name
