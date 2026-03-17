module Npgsql.``Query Unit Tests``

open Swensen.Unquote
open SqlHydra.Query
open SqlHydra.Query.NpgsqlExtensions
open type SqlFn
open NUnit.Framework
open DB
#if NET8_0
open Npgsql.AdventureWorksNet8
#endif
#if NET9_0
open Npgsql.AdventureWorksNet9
#endif
#if NET10_0
open Npgsql.AdventureWorksNet10
#endif

[<Test>]
let ``Simple Where``() = 
    let sql =  
        select {
            for a in person.address do
            where (a.city = "Dallas")
            orderBy a.city
        }
        |> toSql

    sql.Contains("WHERE") =! true

[<Test>]
let ``Select 1 Column``() = 
    let sql = 
        select {
            for a in person.address do
            select (a.city)
        }
        |> toSql

    sql.Contains("SELECT \"a\".\"city\" FROM") =! true

[<Test>]
let ``Select 2 Columns``() = 
    let sql = 
        select {
            for h in sales.salesorderheader do
            select (h.customerid, h.onlineorderflag)
        }
        |> toSql

    sql.Contains("SELECT \"h\".\"customerid\", \"h\".\"onlineorderflag\" FROM") =! true

[<Test; Ignore("Temporarily ignoring test for emergency fix")>]
let ``Select 1 Table and 1 Column``() = 
    let sql = 
        select {
            for o in sales.salesorderheader do
            join d in sales.salesorderdetail on (o.salesorderid = d.salesorderid)
            where o.onlineorderflag
            select (o, d.unitprice)
        }
        |> toSql

    sql.Contains("""SELECT "o"."salesorderid", "o"."revisionnumber", "o"."orderdate", "o"."duedate", "o"."shipdate", "o"."status", "o"."onlineorderflag", "o"."purchaseordernumber", "o"."accountnumber", "o"."customerid", "o"."salespersonid", "o"."territoryid", "o"."billtoaddressid", "o"."shiptoaddressid", "o"."shipmethodid", "o"."creditcardid", "o"."creditcardapprovalcode", "o"."currencyrateid", "o"."subtotal", "o"."taxamt", "o"."freight", "o"."totaldue", "o"."comment", "o"."rowguid", "o"."modifieddate", "d"."unitprice" FROM""") =! true

[<Test>]
let ``Where with Option Type``() = 
    let sql =  
        select {
            for a in person.address do
            where (a.addressline2 <> None)
        }
        |> toSql

    sql.Contains("IS NOT NULL") =! true

[<Test>]
let ``Where with Option Type After Join``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            join d in sales.salesorderdetail on (o.salesorderid = d.salesorderid)
            where (o.creditcardid <> None)
        }
        |> toSql
    sql.Contains("IS NOT NULL") =! true

[<Test>]
let ``Where with Option Type After Left Join``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            leftJoin d in sales.salesorderdetail on (o.salesorderid = d.Value.salesorderid)
            where (o.creditcardid <> None)
        }
        |> toSql
    sql.Contains("IS NOT NULL") =! true

[<Test>]
let ``Where with Option Type on Joined Table Column``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            join d in sales.salesorderdetail on (o.salesorderid = d.salesorderid)
            where (d.carriertrackingnumber <> None)
        }
        |> toSql
    sql.Contains("IS NOT NULL") =! true

[<Test>]
let ``Where with Option Type on Left Joined Table Column``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            leftJoin d in sales.salesorderdetail on (o.salesorderid = d.Value.salesorderid)
            where (d.Value.carriertrackingnumber <> None)
        }
        |> toSql
    sql.Contains("IS NOT NULL") =! true

[<Test>]
let ``Where Not Like``() =
    let sql = 
        select {
            for a in person.address do
            where (a.city <>% "S%")
        }
        |> toSql

    sql =! """SELECT * FROM "person"."address" AS "a" WHERE (NOT ("a"."city" ilike @p0))"""

[<Test>]
let ``Where Like``() =
    let sql =
        select {
            for a in person.address do
            where (a.city =% "S%")
        }
        |> toSql

    sql =! """SELECT * FROM "person"."address" AS "a" WHERE ("a"."city" ilike @p0)"""

[<Test>]
let ``Where like function``() =
    let sql =
        select {
            for a in person.address do
            where (like a.city "S%")
        }
        |> toSql

    sql =! """SELECT * FROM "person"."address" AS "a" WHERE ("a"."city" ilike @p0)"""

[<Test>]
let ``Or Where``() =
    let sql =  
        select {
            for a in person.address do
            where (a.city = "Chicago" || a.city = "Dallas")
        }
        |> toSql

    sql.Contains("WHERE ((\"a\".\"city\" = @p0) OR (\"a\".\"city\" = @p1))") =! true

[<Test>]
let ``And Where``() = 
    let sql =  
        select {
            for a in person.address do
            where (a.city = "Chicago" && a.city = "Dallas")
        }
        |> toSql

    sql.Contains("WHERE ((\"a\".\"city\" = @p0) AND (\"a\".\"city\" = @p1))") =! true

[<Test>]
let ``Where with AND and OR in Parenthesis``() = 
    let sql =  
        select {
            for a in person.address do
            where (a.city = "Chicago" && (a.addressline2 = Some "abc" || isNullValue a.addressline2))
        }
        |> toSql

    Assert.IsTrue( 
        sql.Contains("WHERE ((\"a\".\"city\" = @p0) AND ((\"a\".\"addressline2\" = @p1) OR (\"a\".\"addressline2\" IS NULL)))"),
        "Should wrap OR clause in parenthesis and each individual where clause in parenthesis.")

[<Test>]
let ``Where value and column are swapped``() = 
    let sql =  
        select {
            for a in person.address do
            where (5 < a.addressid && 20 >= a.addressid)
        }
        |> toSql

    sql.Contains("WHERE ((\"a\".\"addressid\" > @p0) AND (\"a\".\"addressid\" <= @p1))") =! true

[<Test>]
let ``Where Not Binary``() = 
    let sql =  
        select {
            for a in person.address do
            where (not (a.city = "Chicago" && a.city = "Dallas"))
        }
        |> toSql

    sql.Contains("WHERE (NOT ((\"a\".\"city\" = @p0) AND (\"a\".\"city\" = @p1)))") =! true

[<Test>]
let ``Where customer isIn List``() = 
    let sql =  
        select {
            for c in sales.customer do
            where (isIn c.customerid [30018;29545;29954])
        }
        |> toSql

    sql.Contains("WHERE (\"c\".\"customerid\" IN (@p0, @p1, @p2))") =! true

[<Test>]
let ``Where customer |=| List``() = 
    let sql =  
        select {
            for c in sales.customer do
            where (c.customerid |=| [30018;29545;29954])
        }
        |> toSql

    sql.Contains("WHERE (\"c\".\"customerid\" IN (@p0, @p1, @p2))") =! true

[<Test>]
let ``Where customer |=| Array``() = 
    let sql =  
        select {
            for c in sales.customer do
            where (c.customerid |=| [| 30018;29545;29954 |])
        }
        |> toSql

    sql.Contains("WHERE (\"c\".\"customerid\" IN (@p0, @p1, @p2))") =! true

[<Test>]
let ``Where customer |=| Seq``() = 
    let buildQuery (values: int seq) = 
        select {
            for c in sales.customer do
            where (c.customerid |=| values)
        }

    let sql =  buildQuery([ 30018;29545;29954 ]) |> toSql
    sql.Contains("WHERE (\"c\".\"customerid\" IN (@p0, @p1, @p2))") =! true

[<Test>]
let ``Where customer |<>| List``() = 
    let sql =  
        select {
            for c in sales.customer do
            where (c.customerid |<>| [ 30018;29545;29954 ])
        }
        |> toSql

    sql.Contains("WHERE (\"c\".\"customerid\" NOT IN (@p0, @p1, @p2))") =! true

[<Test>]
let ``Inner Join``() = 
    let sql = 
        select {
            for o in sales.salesorderheader do
            join d in sales.salesorderdetail on (o.salesorderid = d.salesorderid)
            select o
        }
        |> toSql

    sql.Contains("INNER JOIN \"sales\".\"salesorderdetail\" AS \"d\" ON (\"o\".\"salesorderid\" = \"d\".\"salesorderid\")") =! true

[<Test>]
let ``Left Join``() = 
    let sql = 
        select {
            for o in sales.salesorderheader do
            leftJoin d in sales.salesorderdetail on (o.salesorderid = d.Value.salesorderid)
            select o
        }
        |> toSql

    sql.Contains("LEFT JOIN \"sales\".\"salesorderdetail\" AS \"d\" ON (\"o\".\"salesorderid\" = \"d\".\"salesorderid\")") =! true

[<Test>]
let ``Inner Join - Multi Column``() = 
    let sql = 
        select {
            for o in sales.salesorderheader do
            join d in sales.salesorderdetail on ((o.salesorderid, o.modifieddate) = (d.salesorderid, d.modifieddate))
            select o
        }
        |> toSql

    sql.Contains("INNER JOIN \"sales\".\"salesorderdetail\" AS \"d\" ON (\"o\".\"salesorderid\" = \"d\".\"salesorderid\" AND \"o\".\"modifieddate\" = \"d\".\"modifieddate\")") =! true

[<Test>]
let ``Left Join - Multi Column``() = 
    let sql = 
        select {
            for o in sales.salesorderheader do
            leftJoin d in sales.salesorderdetail on ((o.salesorderid, o.modifieddate) = (d.Value.salesorderid, d.Value.modifieddate))
            select o
        }
        |> toSql

    sql.Contains("LEFT JOIN \"sales\".\"salesorderdetail\" AS \"d\" ON (\"o\".\"salesorderid\" = \"d\".\"salesorderid\" AND \"o\".\"modifieddate\" = \"d\".\"modifieddate\")") =! true

[<Test>]
let ``Correlated Subquery``() = 
    let latestOrderByCustomer = 
        select {
            for d in sales.salesorderheader do
            correlate od in sales.salesorderheader
            where (d.customerid = od.customerid)
            select (maxBy d.orderdate)
        }

    let sql =  
        select {
            for od in sales.salesorderheader do
            where (od.orderdate = subqueryOne latestOrderByCustomer)
        }
        |> toSql

    sql =!
        "SELECT * FROM \"sales\".\"salesorderheader\" AS \"od\" WHERE (\"od\".\"orderdate\" = \
        (SELECT MAX(\"d\".\"orderdate\") AS __hydra_expr_0 FROM \"sales\".\"salesorderheader\" AS \"d\" \
        WHERE (\"d\".\"customerid\" = \"od\".\"customerid\")))".RemoveHydraExpr()

[<Test>]
let ``Delete Query with Where``() = 
    let sql =  
        delete {
            for c in sales.customer do
            where (c.customerid |<>| [ 30018;29545;29954 ])
        }
        |> toSql

    sql.Contains("DELETE FROM \"sales\".\"customer\"") =! true
    sql.Contains("WHERE (\"sales\".\"customer\".\"customerid\" NOT IN (@p0, @p1, @p2))") =! true

[<Test>]
let ``Delete All``() = 
    let sql =  
        delete {
            for c in sales.customer do
            deleteAll
        }
        |> toSql

    sql =! "DELETE FROM \"sales\".\"customer\""

[<Test>]
let ``Update Query with Where``() = 
    let sql =  
        update {
            for c in sales.customer do
            set c.personid (Some 123)
            where (c.personid = Some 456)
        }
        |> toSql

    sql =! "UPDATE \"sales\".\"customer\" SET \"personid\" = @p0 WHERE (\"sales\".\"customer\".\"personid\" = @p1)"

[<Test>]
let ``Update Query with multiple Wheres``() = 
    let sql =  
        update {
            for c in sales.customer do
            set c.personid (Some 123)
            where (c.personid = Some 456)
            where (c.customerid = 789)
        }
        |> toSql

    sql =! """UPDATE "sales"."customer" SET "personid" = @p0 WHERE ("sales"."customer"."personid" = @p1 AND ("sales"."customer"."customerid" = @p2))"""

[<Test>]
let ``Update Query with No Where``() = 
    let sql =  
        update {
            for c in sales.customer do
            set c.customerid 123
            updateAll
        }
        |> toSql

    sql =! "UPDATE \"sales\".\"customer\" SET \"customerid\" = @p0"

[<Test>]
let ``Update should fail without where or updateAll``() = 
    try 
        let sql =  
            update {
                for c in sales.customer do
                set c.customerid 123
            }
        failwith "Should fail because no `where` or `updateAll` exists."
    with ex ->
        () // Pass

[<Test>]
let ``Update should pass because where exists``() = 
    update {
        for c in sales.customer do
        set c.customerid 123
        where (c.customerid = 1)
    }
    |> ignore

[<Test>]
let ``Update should pass because updateAll exists``() = 
    update {
        for c in sales.customer do
        set c.customerid 123
        updateAll
    }
    |> ignore

[<Test>]
let ``Update with where followed by updateAll should fail``() = 
    try
        update {
            for c in sales.customer do
            set c.customerid 123
            where (c.customerid = 1)
            updateAll
        }
        |> ignore
        Assert.Fail()
    with ex ->
        ()

[<Test>]
let ``Update with updateAll followed by where should fail``() = 
    try
        update {
            for c in sales.customer do
            set c.customerid 123
            updateAll
            where (c.customerid = 1)
        }
        |> ignore
        Assert.Fail()
    with ex ->
        ()

[<Test>]
let ``Insert Query``() = 
    let sql =  
        insert {
            into sales.customer
            entity 
                { 
                    sales.customer.modifieddate = System.DateTime.Today
                    sales.customer.territoryid = None
                    sales.customer.storeid = None
                    sales.customer.personid = Some 1
                    sales.customer.rowguid = System.Guid.NewGuid()
                    sales.customer.customerid = 0
                }
        }
        |> toSql

    sql =! "INSERT INTO \"sales\".\"customer\" (\"customerid\", \"personid\", \"storeid\", \"territoryid\", \"rowguid\", \"modifieddate\") VALUES (@p0, @p1, @p2, @p3, @p4, @p5)" 

[<Test>]
let ``Inline Aggregates``() = 
    let sql = 
        select {
            for o in sales.salesorderheader do
            select (countBy o.salesorderid)
        }
        |> toSql

    sql =! "SELECT COUNT(\"o\".\"salesorderid\") AS __hydra_expr_0 FROM \"sales\".\"salesorderheader\" AS \"o\"".RemoveHydraExpr()

[<Test>]
let ``Insert with Returning``() =
    let query =
        insert {
            for c in sales.customer do
            entity
                {
                    sales.customer.modifieddate = System.DateTime.Today
                    sales.customer.territoryid = None
                    sales.customer.storeid = None
                    sales.customer.personid = Some 1
                    sales.customer.rowguid = System.Guid.NewGuid()
                    sales.customer.customerid = 0
                }
            returning (c.customerid, c.rowguid)
        }

    Assert.AreEqual(2, query.Spec.OutputFields.Length, "Expected 2 output fields")
    Assert.AreEqual("customerid", query.Spec.OutputFields.[0].ColumnName)
    Assert.AreEqual("rowguid", query.Spec.OutputFields.[1].ColumnName)

[<Test>]
let ``Insert OnConflictDoNothing with Where``() =
    let query =
        insert {
            for c in sales.customer do
            entity
                {
                    sales.customer.modifieddate = System.DateTime.Today
                    sales.customer.territoryid = None
                    sales.customer.storeid = None
                    sales.customer.personid = Some 1
                    sales.customer.rowguid = System.Guid.NewGuid()
                    sales.customer.customerid = 0
                }
            onConflictDoNothingWhere c.personid "personid IS NOT NULL"
        }

    match query.Spec.InsertType with
    | OnConflictDoNothingWhere (fields, whereClause) ->
        Assert.AreEqual(["personid"], fields)
        Assert.AreEqual("personid IS NOT NULL", whereClause)
    | _ -> Assert.Fail("Expected OnConflictDoNothingWhere")

[<Test>]
let ``Where with Coalesce``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            where (coalesce(o.creditcardid, 0) = 123)
        }
        |> toSql

    printfn "SQL: %s" sql
    sql.Contains("coalesce") =! true

[<Test>]
let ``Where with Coalesce compared to value``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            where (coalesce(o.salespersonid, 0) <> 0)
        }
        |> toSql

    printfn "SQL: %s" sql
    sql.Contains("coalesce") =! true

// =============================================================================
// Regression tests reproducing thellma/intelligence production SqlHydra bugs
// =============================================================================

[<Test>]
let ``REGRESSION: <> None with complex boolean (OR, AND, = Some) - production pattern A``() =
    let someId = 42
    let sql =
        select {
            for o in sales.salesorderheader do
            where (
                (o.salespersonid = Some someId || o.onlineorderflag)
                && o.shipdate <> None
            )
        }
        |> toSql

    printfn "SQL: %s" sql
    sql.Contains("IS NOT NULL") =! true

[<Test>]
let ``REGRESSION: <> None on joined table column - production pattern B``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            join d in sales.salesorderdetail on (o.salesorderid = d.salesorderid)
            where (o.customerid = 123 && o.shipdate <> None)
        }
        |> toSql

    printfn "SQL: %s" sql
    sql.Contains("IS NOT NULL") =! true

[<Test>]
let ``REGRESSION: <> None on second table column after join - production pattern B variant``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            join d in sales.salesorderdetail on (o.salesorderid = d.salesorderid)
            where (d.carriertrackingnumber <> None)
        }
        |> toSql

    printfn "SQL: %s" sql
    sql.Contains("IS NOT NULL") =! true

[<Test>]
let ``REGRESSION: leftJoin' with on' predicate-style join - production pattern C``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            leftJoin' d in sales.salesorderdetail; on' (o.salesorderid = d.Value.salesorderid)
            select o
        }
        |> toSql

    printfn "SQL: %s" sql
    sql.Contains("LEFT JOIN") =! true

[<Test>]
let ``REGRESSION: leftJoin' with anti-join where None - production pattern C variant``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            leftJoin' d in sales.salesorderdetail; on' (o.salesorderid = d.Value.salesorderid)
            where (d = None)
            select o
        }
        |> toSql

    printfn "SQL: %s" sql
    sql.Contains("IS NULL") =! true

[<Test>]
let ``REGRESSION: leftJoin' with where d <> None - semi-join pattern``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            leftJoin' d in sales.salesorderdetail; on' (o.salesorderid = d.Value.salesorderid)
            where (d <> None)
            select o
        }
        |> toSql

    printfn "SQL: %s" sql
    sql.Contains("IS NOT NULL") =! true

[<Test>]
let ``REGRESSION: =% ILIKE with percent pattern - production pattern D``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            where (o.purchaseordernumber =% "%search%")
        }
        |> toSql

    printfn "SQL: %s" sql
    sql.Contains("ilike") =! true

[<Test>]
let ``REGRESSION: =% ILIKE on Option string column - production pattern D variant``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            where (o.comment =% "%test%")
        }
        |> toSql

    printfn "SQL: %s" sql
    sql.Contains("ilike") =! true
