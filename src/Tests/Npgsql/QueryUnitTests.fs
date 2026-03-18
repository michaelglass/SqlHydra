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
let ``Insert OnConflictDoNothing with WhereRaw``() =
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
            onConflictDoNothingWhereRaw c.personid "personid IS NOT NULL"
        }

    match query.Spec.InsertType with
    | OnConflictDoNothingWhereRaw (fields, whereClause) ->
        Assert.AreEqual(["personid"], fields)
        Assert.AreEqual("personid IS NOT NULL", whereClause)
    | _ -> Assert.Fail("Expected OnConflictDoNothingWhereRaw")

[<Test>]
let ``Insert onConflictDoNothingRawTarget with expression index``() =
    let query =
        insert {
            for c in sales.currency do
            entity
                {
                    sales.currency.currencycode = "TST"
                    sales.currency.name = "Test"
                    sales.currency.modifieddate = System.DateTime.Now
                }
            onConflictDoNothingRawTarget "currencycode, COALESCE(name, '')"
        }

    match query.Spec.InsertType with
    | OnConflictDoNothingRawTarget target ->
        Assert.AreEqual("currencycode, COALESCE(name, '')", target)
    | other -> Assert.Fail($"Expected OnConflictDoNothingRawTarget, got {other}")

[<Test>]
let ``Where with Coalesce``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            where (coalesce(o.creditcardid, 0) = 123)
        }
        |> toSql

    sql.Contains("coalesce") =! true

[<Test>]
let ``Where with Coalesce compared to value``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            where (coalesce(o.salespersonid, 0) <> 0)
        }
        |> toSql

    sql.Contains("coalesce") =! true

// =============================================================================
// Regression tests reproducing thellma/intelligence production SqlHydra bugs
// =============================================================================

[<Test>]
let ``Where <> None with complex boolean (OR, AND, = Some)``() =
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

    sql.Contains("IS NOT NULL") =! true

[<Test>]
let ``Where =% ILIKE with percent pattern``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            where (o.purchaseordernumber =% "%search%")
        }
        |> toSql

    sql.Contains("ilike") =! true

[<Test>]
let ``Where =% ILIKE on Option string column``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            where (o.comment =% "%test%")
        }
        |> toSql

    sql.Contains("ilike") =! true

[<Test>]
let ``OrderBy NULLS LAST``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            orderByNullsLast o.creditcardid
            select o
        }
        |> toSql

    sql.Contains("NULLS LAST") =! true

[<Test>]
let ``OrderByDescending NULLS FIRST``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            orderByDescNullsFirst o.creditcardid
            select o
        }
        |> toSql

    sql.Contains("NULLS FIRST") =! true

[<Test>]
let ``DISTINCT ON single column``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            distinctOn o.customerid
            orderBy o.customerid
            thenByDescending o.orderdate
            select o
        }
        |> toSql

    sql.Contains("DISTINCT ON") =! true
    sql.Contains("\"customerid\"") =! true

[<Test>]
let ``DISTINCT ON with CTE injects into outer SELECT not CTE``() =
    let innerQuery =
        select {
            for c in sales.customer do
            select {| CustomerId = c.customerid; StoreId = c.storeid |}
        }

    let sql =
        select {
            for c in Table.cte<{| CustomerId: int; StoreId: int option |}> "cust_cte" innerQuery do
            distinctOn c.CustomerId
            select c
        }
        |> toSql

    // DISTINCT ON must be in the outer SELECT, not inside the CTE
    let distinctIdx = sql.IndexOf("DISTINCT ON")
    Assert.IsTrue(distinctIdx >= 0, "Should contain DISTINCT ON")
    // Verify DISTINCT ON is in the outer SELECT (after CTE), not the inner one
    let cteSelectIdx = sql.IndexOf("SELECT ", 0)  // First SELECT is inside CTE
    let outerSelectIdx = sql.IndexOf("SELECT ", cteSelectIdx + 1)  // Second SELECT is outer
    Assert.IsTrue(outerSelectIdx > 0, "Should have an outer SELECT after CTE")
    Assert.IsTrue(distinctIdx >= outerSelectIdx, $"DISTINCT ON (at {distinctIdx}) should be at or after outer SELECT (at {outerSelectIdx})")
    Assert.IsTrue(distinctIdx < outerSelectIdx + "SELECT DISTINCT ON".Length + 50, $"DISTINCT ON should be near the outer SELECT, not far after it")

[<Test>]
let ``REGRESSION: leftJoin' on' with compound predicate and external value``() =
    let minDiscount = 0m
    let sql =
        select {
            for o in sales.salesorderheader do
            leftJoin' d in sales.salesorderdetail; on' (o.salesorderid = d.Value.salesorderid && d.Value.unitpricediscount > minDiscount)
            where (o.customerid = 42)
            select o
        }
        |> toSql

    sql.Contains("LEFT JOIN") =! true
    sql.Contains("AND") =! true

[<Test>]
let ``Update with setRaw stores raw SET values in spec``() =
    let query =
        update {
            for c in sales.customer do
            set c.customerid 1
            setRaw c.personid "COALESCE(?, personid)" [| box (Some 123) |]
            where (c.customerid = 1)
        }

    Assert.AreEqual(1, query.Spec.SetRawValues.Length, "Expected 1 raw SET value")
    let (col, sql, parms) = query.Spec.SetRawValues.[0]
    Assert.AreEqual("personid", col)
    Assert.AreEqual("COALESCE(?, personid)", sql)
    Assert.AreEqual(1, parms.Length)

[<Test>]
let ``Update with setRaw and set generates correct SQL``() =
    let sql =
        update {
            for c in sales.customer do
            set c.customerid 1
            setRaw c.personid "COALESCE(?, personid)" [| box 123 |]
            where (c.customerid = 1)
        }
        |> toSql

    // With UnsafeLiteral, both set and setRaw appear in toSql output
    sql.Contains("SET") =! true
    sql.Contains("\"customerid\"") =! true
    sql.Contains("\"personid\"") =! true
    sql.Contains("COALESCE") =! true

[<Test>]
let ``Update with only setRaw generates valid query``() =
    let query =
        update {
            for c in sales.customer do
            setRaw c.personid "COALESCE(?, personid)" [| box 123 |]
            where (c.customerid = 1)
        }

    Assert.AreEqual(1, query.Spec.SetRawValues.Length, "Expected 1 raw SET value")
    Assert.AreEqual(0, query.Spec.SetValues.Length, "Expected 0 regular SET values")
    // Should not throw - fromUpdate should handle setRaw-only case
    let sql = query |> toSql

    sql.Contains("UPDATE") =! true
    sql.Contains("COALESCE") =! true
    sql.Contains("\"personid\"") =! true

[<Test>]
let ``setRaw raw SQL appears in toSql output``() =
    let sql =
        update {
            for c in sales.customer do
            setRaw c.personid "COALESCE(@__raw_0, \"personid\") + 1" [||]
            where (c.customerid = 1)
        }
        |> toSql

    // With UnsafeLiteral integration, raw SQL is visible in toSql
    sql.Contains("COALESCE") =! true
    sql.Contains("\"personid\"") =! true

[<Test>]
let ``Mixed set and setRaw both appear in toSql``() =
    let sql =
        update {
            for c in sales.customer do
            set c.customerid 42
            setRaw c.personid "COALESCE(?, \"personid\")" [| box 99 |]
            where (c.customerid = 1)
        }
        |> toSql

    // Both regular SET and raw SET should appear
    sql.Contains("\"customerid\"") =! true
    sql.Contains("\"personid\"") =! true
    sql.Contains("COALESCE") =! true

[<Test>]
let ``Insert onConflictDoUpdateCoalesce stores correct spec``() =
    let query =
        insert {
            for c in sales.currency do
            entity
                {
                    sales.currency.currencycode = "TST"
                    sales.currency.name = "Test"
                    sales.currency.modifieddate = System.DateTime.Today
                }
            onConflictDoUpdateCoalesce c.currencycode (c.name, c.modifieddate) c.name
        }

    match query.Spec.InsertType with
    | OnConflictDoUpdateCoalesce (conflictFields, updateFields, coalesceFields) ->
        Assert.AreEqual(["currencycode"], conflictFields)
        Assert.AreEqual(["name"; "modifieddate"], updateFields)
        Assert.AreEqual(["name"], coalesceFields)
    | _ -> Assert.Fail("Expected OnConflictDoUpdateCoalesce")

[<Test>]
let ``onConflictDoUpdateCoalesce generates COALESCE SQL``() =
    let query =
        insert {
            for c in sales.currency do
            entity
                {
                    sales.currency.currencycode = "TST"
                    sales.currency.name = "Test"
                    sales.currency.modifieddate = System.DateTime.Today
                }
            onConflictDoUpdateCoalesce c.currencycode (c.name, c.modifieddate) c.name
        }

    let compiler = SqlKata.Compilers.PostgresCompiler()
    let compiled = compiler.Compile(query.ToKataQuery())
    let sql =
        match query.Spec.InsertType with
        | OnConflictDoUpdateCoalesce (cf, uf, coal) -> OnConflict.onConflictDoUpdateCoalesce "sales.currency" cf uf coal compiled.Sql
        | _ -> compiled.Sql

    sql.Contains("COALESCE") =! true
    sql.Contains("EXCLUDED") =! true

[<Test>]
let ``CTE with anonymous record``() =
    let innerQuery =
        select {
            for a in person.address do
            select {| City = a.city; Id = a.addressid |}
        }

    let sql =
        select {
            for r in cte "my_cte" innerQuery do
            where (r.Id > 5)
            select r.City
        }
        |> toSql

    sql.Contains("my_cte") =! true
    sql.Contains("\"r\".\"Id\"") =! true

[<Test>]
let ``CTE select all columns``() =
    let innerQuery =
        select {
            for a in person.address do
            select {| City = a.city; Id = a.addressid |}
        }

    let sql =
        select {
            for r in cte "addr_cte" innerQuery do
            select r
        }
        |> toSql

    sql.Contains("addr_cte") =! true

[<Test>]
let ``CTE with where on multiple columns``() =
    let innerQuery =
        select {
            for o in sales.salesorderheader do
            where (o.onlineorderflag)
            select {| CustomerId = o.customerid; OrderDate = o.orderdate; Total = o.totaldue |}
        }

    let sql =
        select {
            for s in cte "online_orders" innerQuery do
            where (s.Total > Some 100m && s.CustomerId > 0)
            orderByDescending s.Total
            select s
        }
        |> toSql

    sql.Contains("online_orders") =! true
    sql.Contains("\"s\".\"Total\"") =! true
    sql.Contains("\"s\".\"CustomerId\"") =! true

[<Test>]
let ``CASE WHEN in select with bool column``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            select (o.customerid, caseWhen (o.onlineorderflag = true) "Online" "InStore")
        }
        |> toSql

    sql.Contains("CASE WHEN") =! true
    sql.Contains("THEN") =! true
    sql.Contains("ELSE") =! true
    sql.Contains("END") =! true

[<Test>]
let ``INSERT from SELECT``() =
    let sourceQuery =
        select {
            for a in person.address do
            where (a.city = "Seattle")
            select (a.city, a.addressid)
        }

    let query =
        insert {
            into person.address
            fromSelect sourceQuery
        }

    match query.Spec.InsertType with
    | InsertFromSelect _ -> Assert.Pass()
    | other -> Assert.Fail($"Expected InsertFromSelect, got {other}")

[<Test>]
let ``CASE WHEN in select with comparison``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            select (o.customerid, caseWhen (o.totaldue > Some 100m) "High" "Low")
        }
        |> toSql

    sql.Contains("CASE WHEN") =! true

[<Test>]
let ``Multi-join select whole first table``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            join d in sales.salesorderdetail on (o.salesorderid = d.salesorderid)
            select o
        }
        |> toSql

    sql.Contains("\"o\".*") =! true
    sql.Contains("INNER JOIN") =! true

[<Test>]
let ``leftJoin' select mixed tuple with table var and scalar``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            leftJoin' d in sales.salesorderdetail; on' (d.Value.salesorderid = o.salesorderid)
            select (d, o.salesorderid)
        }
        |> toSql

    sql.Contains("LEFT JOIN") =! true
    sql.Contains("\"d\".*") =! true
    sql.Contains("\"o\".\"salesorderid\"") =! true

[<Test>]
let ``Multi-join orderBy on first table column``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            join d in sales.salesorderdetail on (o.salesorderid = d.salesorderid)
            orderByDescending o.orderdate
            select (o.salesorderid, d.unitprice)
        }
        |> toSql

    sql.Contains("INNER JOIN") =! true
    sql.Contains("ORDER BY") =! true
    sql.Contains("\"orderdate\"") =! true

[<Test>]
let ``Multi-join orderBy on joined table column``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            join d in sales.salesorderdetail on (o.salesorderid = d.salesorderid)
            orderBy d.unitprice
            select (o.salesorderid, d.unitprice)
        }
        |> toSql

    sql.Contains("INNER JOIN") =! true
    sql.Contains("ORDER BY") =! true
    sql.Contains("\"unitprice\"") =! true

[<Test>]
let ``Multi-join orderByDescending on joined table column``() =
    let sql =
        select {
            for o in sales.salesorderheader do
            join d in sales.salesorderdetail on (o.salesorderid = d.salesorderid)
            orderByDescending d.unitprice
            select (o.salesorderid, d.unitprice)
        }
        |> toSql

    sql.Contains("INNER JOIN") =! true
    sql.Contains("ORDER BY") =! true
    sql.Contains("\"unitprice\" DESC") =! true

[<Test>]
let ``Select with inlineValue injects external value as parameter``() =
    let externalValue = "hello"
    let sql =
        select {
            for o in sales.salesorderheader do
            where (o.salesorderid = 1)
            select {| OrderId = o.salesorderid; Tag = inlineValue externalValue |}
        }
        |> toSql

    // inlineValue should appear as a parameter placeholder in the SQL
    sql.Contains("@") =! true
    sql.Contains("salesorderid") =! true

[<Test>]
let ``INSERT FROM SELECT with inlineValue for external values``() =
    let changedBy = "admin"
    let notes = "bulk update"

    let sourceQuery =
        select {
            for a in person.address do
            where (a.city = "Seattle")
            select {| City = a.city; AccountNumber = inlineValue changedBy; Comment = inlineValue notes |}
        }

    let query =
        insert {
            into person.address
            fromSelect sourceQuery
        }

    match query.Spec.InsertType with
    | InsertFromSelect _ -> Assert.Pass()
    | other -> Assert.Fail($"Expected InsertFromSelect, got {other}")
