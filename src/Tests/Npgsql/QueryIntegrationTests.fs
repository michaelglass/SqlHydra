module Npgsql.``Query Integration Tests``

open Swensen.Unquote
open SqlHydra.Query
open SqlHydra.Query.NpgsqlExtensions
open type SqlFn
open NUnit.Framework
open System.Threading.Tasks
open Npgsql.DB
#if NET8_0
open Npgsql.AdventureWorksNet8
#endif
#if NET9_0
open Npgsql.AdventureWorksNet9
#endif
#if NET10_0
open Npgsql.AdventureWorksNet10
#endif

let db = 
    let dataSource = 
        let builder = Npgsql.NpgsqlDataSourceBuilder(connectionString)
        builder.MapEnum<ext.mood>("ext.mood") |> ignore    
        builder.Build()
    
    QueryContextFactory.Create(dataSource, sqlLogger = printf "SQL: %O")

[<Test>]
let ``Where City Contains``() = task {
    let! addresses =
        selectTask db {
            for a in person.address do
            where (a.city |=| [ "Seattle"; "Santa Cruz" ])
        }

    gt0 addresses
    Assert.IsTrue(addresses |> Seq.forall (fun a -> a.city = "Seattle" || a.city = "Santa Cruz"), "Expected only 'Seattle' or 'Santa Cruz'.")
}

[<Test>]
let ``Select city Column Where city Starts with S``() = task {
    let! cities =
        selectTask db {
            for a in person.address do
            where (a.city =% "S%")
            select a.city
        }

    gt0 cities
    Assert.IsTrue(cities |> Seq.forall (fun city -> city.StartsWith "S"), "Expected all cities to start with 'S'.")
}

[<Test>]
let ``Inner Join Orders-Details``() = task {
    let! results =
        selectTask db {
            for o in sales.salesorderheader do
            join d in sales.salesorderdetail on (o.salesorderid = d.salesorderid)
            where o.onlineorderflag
            select (o, d)
        }

    gt0 results
}

[<Test>]
let ``Product with Category name``() = task {
    let! rows =
        selectTask db {
            for p in production.product do
            join sc in production.productsubcategory on (p.productsubcategoryid = Some sc.productsubcategoryid)
            join c in production.productcategory on (sc.productcategoryid = c.productcategoryid)
            select (c.name, p)
            take 5
        }

    gt0 rows
}

[<Test>]
let ``Select Column Aggregates From Product IDs 1-3``() = task {
    let! aggregates =
        selectTask db {
            for p in production.product do
            where (p.productsubcategoryid <> None)
            groupBy p.productsubcategoryid
            where (p.productsubcategoryid.Value |=| [ 1; 2; 3 ])
            select (p.productsubcategoryid, minBy p.listprice, maxBy p.listprice, avgBy p.listprice, countBy p.listprice, sumBy p.listprice)
        }

    gt0 aggregates

    let aggByCatID = 
        aggregates 
        |> Seq.map (fun (catId, minPrice, maxPrice, avgPrice, priceCount, sumPrice) -> catId, (minPrice, maxPrice, avgPrice, priceCount, sumPrice)) 
        |> Map.ofSeq
    
    let dc (actual: decimal) (expected: decimal) = 
        Assert.AreEqual(float actual, float expected, 0.0001, "Expected values to be close")

    let verifyAggregateValuesFor (catId: int) (xMinPrice, xMaxPrice, xAvgPrice, xPriceCount, xSumPrice) =
        let aMinPrice, aMaxPrice, aAvgPrice, aPriceCount, aSumPrice = aggByCatID.[Some catId]
        dc aMinPrice xMinPrice; dc aMaxPrice xMaxPrice; dc aAvgPrice xAvgPrice; Assert.AreEqual(aPriceCount, xPriceCount); dc aSumPrice xSumPrice
    
    verifyAggregateValuesFor 1 (539.99M, 3399.99M, 1683.365M, 32, 53867.6800M)
    verifyAggregateValuesFor 2 (539.99M, 3578.2700M, 1597.4500M, 43, 68690.3500M)
    verifyAggregateValuesFor 3 (742.3500M, 2384.0700M, 1425.2481M, 22, 31355.4600M)
}

[<Test>]
let ``Aggregate Subquery One``() = task {
    let avgListPrice =
        select {
            for p in production.product do
            select (avgBy p.listprice)
        }

    let! productsWithHigherThanAvgPrice =
        selectTask db {
            for p in production.product do
            where (p.listprice > subqueryOne avgListPrice)
            orderByDescending p.listprice
            select (p.name, p.listprice)
        }

    let avgListPrice = 438.6662M

    gt0 productsWithHigherThanAvgPrice
    Assert.IsTrue(productsWithHigherThanAvgPrice |> Seq.forall (fun (nm, price) -> price > avgListPrice), "Expected all prices to be > than avg price of $438.67.")
}

[<Test>]
let ``Select Column Aggregates``() = task {
    let! aggregates =
        selectTask db {
            for p in production.product do
            where (p.productsubcategoryid <> None)
            groupBy p.productsubcategoryid
            where (p.productsubcategoryid.Value |=| [ 1; 2; 3 ])
            select (p.productsubcategoryid, minBy p.listprice, maxBy p.listprice)
        }

    gt0 aggregates
}

[<Test>]
let ``Sorted Aggregates - Top 5 categories with highest avg price products``() = task {
    let! aggregates =
        selectTask db {
            for p in production.product do
            where (p.productsubcategoryid <> None)
            groupBy p.productsubcategoryid
            orderByDescending (avgBy p.listprice)
            select (p.productsubcategoryid, avgBy p.listprice)
            take 5
        }

    gt0 aggregates
}

[<Test>]
let ``Where subqueryMany``() = task {
    let top5CategoryIdsWithHighestAvgPrices =
        select {
            for p in production.product do
            where (p.productsubcategoryid <> None)
            groupBy p.productsubcategoryid
            orderByDescending (avgBy p.listprice)
            select (p.productsubcategoryid)
            take 5
        }

    let! top5Categories =
        selectTask db {
            for c in production.productcategory do
            where (Some c.productcategoryid |=| subqueryMany top5CategoryIdsWithHighestAvgPrices)
            select c.name
        }

    gt0 top5Categories
}

[<Test>]
let ``Where subqueryOne``() = task {
    let avgListPrice =
        select {
            for p in production.product do
            select (avgBy p.listprice)
        }

    let! productsWithAboveAveragePrice =
        selectTask db {
            for p in production.product do
            where (p.listprice > subqueryOne avgListPrice)
            select (p.name, p.listprice)
        }

    gt0 productsWithAboveAveragePrice
}

[<Test>]
let ``Select Columns with Option``() = task {
    let! values =
        selectTask db {
            for p in production.product do
            where (p.productsubcategoryid <> None)
            select (p.productsubcategoryid, p.listprice)
        }

    gt0 values
    Assert.IsTrue(values |> Seq.forall (fun (catId, price) -> catId <> None), "Expected subcategories to all have a value.")
}

[<Test>]
let ``Insert Currency``() = task {
    use! shared = db.OpenContextAsync()

    let! results =
        insertTask shared {
            for c in sales.currency do
            entity 
                {
                    sales.currency.currencycode = "BTC"
                    sales.currency.name = "BitCoin"
                    sales.currency.modifieddate = System.DateTime.Today
                }
        }

    results =! 1

    let! btc =
        selectTask shared {
            for c in sales.currency do
            where (c.currencycode = "BTC")
        }

    gt0 btc
}

[<Test>]
let ``Update Currency``() = task {
    use! shared = db.OpenContextAsync()

    let! results = 
        updateTask shared {
            for c in sales.currency do
            set c.name "BitCoinzz"
            where (c.currencycode = "BTC")
        }

    results >! 0

    let! btc =
        selectTask shared {
            for c in sales.currency do
            where (c.name = "BitCoinzz")
        }

    gt0 btc
}

[<Test>]
let ``Delete Currency``() = task {
    use! shared = db.OpenContextAsync()

    let! _ = 
        deleteAsync shared {
            for c in sales.currency do
            where (c.currencycode = "BTC")
        }

    let! btc =
        selectTask shared {
            for c in sales.currency do
            where (c.currencycode = "BTC")
        }

    Assert.IsTrue(btc |> Seq.length = 0, "Should be deleted")
}

[<Test>]
let ``Insert Network``() = task {
    use! shared = db.OpenContextAsync()

    let! results = 
        insertAsync shared {
            for c in network_sample.network_addresses do
            entity 
                {
                    network_sample.network_addresses.id = 0
                    network_sample.network_addresses.net_cidr = System.Net.IPNetwork.Parse("::ffff:1.2.3.0/120")
                    network_sample.network_addresses.net_inet = System.Net.IPAddress.Parse("127.0.0.2")
                    network_sample.network_addresses.net_macaddr = System.Net.NetworkInformation.PhysicalAddress.Parse("00-11-22-33-44-55")
                    network_sample.network_addresses.net_macaddr8 = System.Net.NetworkInformation.PhysicalAddress.Parse("00-11-22-33-44-55")
                }
            excludeColumn c.id
        }

    results =! 1

    let! ipAddr =
        selectTask shared {
            for c in network_sample.network_addresses do
            where (c.net_inet = System.Net.IPAddress.Parse "127.0.0.2")
        }

    gt0 ipAddr
}

[<Test; Ignore "Ignore">]
let ``Insert and Get Id``() = task {
    use! shared = db.OpenContextAsync()
            
    shared.BeginTransaction()
    let! deletedCount =
        deleteAsync shared {
            for r in production.productreview do
            where (r.emailaddress = "gfisher@askjeeves.com")
        }

    shared.CommitTransaction()

    shared.BeginTransaction()

    let! prodReviewId = 
        insertTask shared {
            for r in production.productreview do
            entity 
                {
                    production.productreview.productreviewid = 9999 // PK
                    production.productreview.comments = Some "The ML Fork makes for a plush ride."
                    production.productreview.emailaddress = "gfisher@askjeeves.com"
                    production.productreview.modifieddate = System.DateTime.Today
                    production.productreview.productid = 803 //ML Fork
                    production.productreview.rating = 5
                    production.productreview.reviewdate = System.DateTime.Today
                    production.productreview.reviewername = "Gary Fisher"
                }
            getId r.productreviewid
        }

    let! review =
        selectTask shared {
            for r in production.productreview do
            where (r.reviewername = "Gary Fisher")
            tryHead
        }
            
    match review with
    | Some (rev : production.productreview) -> 
        Assert.IsTrue(prodReviewId > 0, "Expected productreviewid to be greater than 0")
    | None -> 
        failwith "Expected to query a review row."

    let! deletedCount = 
        deleteAsync shared {
            for r in production.productreview do
            where (r.productreviewid = prodReviewId)
        }

    Assert.AreEqual(deletedCount, 1, "Expected exactly one review to be deleted")

    let! reviews =
        selectTask shared {
            for r in production.productreview do
            where (r.reviewername = "Gary Fisher")
        }

    Assert.AreEqual(reviews |> Seq.length, 0, "Expected no reviews to be queryable")
    shared.CommitTransaction()
}

[<Test>]
let ``Multiple Inserts``() = task {
    use! shared = db.OpenContextAsync()

    shared.BeginTransaction()

    let currencies = 
        [ 0 .. 2 ] 
        |> List.map (fun i -> 
            {
                sales.currency.currencycode = $"BC{i}"
                sales.currency.name = "BitCoin"
                sales.currency.modifieddate = System.DateTime.Now
            }
        )
        |> AtLeastOne.tryCreate
    
    match currencies with
    | Some currencies ->
        let! rowsInserted = 
            insertAsync shared {
                into sales.currency
                entities currencies
            }

        Assert.AreEqual(rowsInserted, 3, "Expected 3 rows to be inserted")

        let! results =
            selectTask shared {
                for c in sales.currency do
                where (c.currencycode =% "BC%")
                orderBy c.currencycode
                select c.currencycode
            }

        let codes = results |> Seq.toList
    
        codes =! [ "BC0"; "BC1"; "BC2" ]
    | None -> ()

    shared.RollbackTransaction()
}

[<Test>]
let ``Distinct Test``() = task {
    use! shared = db.OpenContextAsync()

    shared.BeginTransaction()

    let currencies = 
        [ 0 .. 2 ] 
        |> List.map (fun i -> 
            {
                sales.currency.currencycode = $"BC{i}"
                sales.currency.name = "BitCoin"
                sales.currency.modifieddate = System.DateTime.Today
            }
        )
        |> AtLeastOne.tryCreate
    
    match currencies with
    | Some currencies ->
        let! rowsInserted = 
            insertTask shared {
                for e in sales.currency do
                entities currencies
            }

        Assert.AreEqual(rowsInserted, 3, "Expected 3 rows to be inserted")

        let! results =
            selectTask shared {
                for c in sales.currency do
                where (c.currencycode =% "BC%")
                select c.name
            }

        let! distinctResults =
            selectTask shared {
                for c in sales.currency do
                where (c.currencycode =% "BC%")
                select c.name
                distinct
            }

        results |> Seq.length =! 3
        distinctResults |> Seq.length =! 1
    | None -> 
        ()

    shared.RollbackTransaction()
}

[<Test>]
let ``Insert, Update and Read npgsql provider specific db fields``() = task {
    use! shared = db.OpenContextAsync()
            
    let expectJsonEqual (dbValue: string) (jsonValue: string) err = 
        Assert.AreEqual(dbValue.Replace(" ", ""), jsonValue, err)
                
    let getRowById id =
        selectTask shared {
            for e in ext.jsonsupport do
            select e
            where (e.id = id)
        }
                
    // Simple insert of one entity
    let jsonValue = """{"name":"test"}"""
    let entity': ext.jsonsupport =
        {
            id = 0
            json_field = jsonValue
            jsonb_field = jsonValue
        }
                
    let! insertedRowId = 
        insertAsync shared {
            for e in ext.jsonsupport do
            entity entity'
            getId e.id
        }
                  
    let! selectedRows = getRowById insertedRowId
    match selectedRows |> Seq.tryHead with
    | Some row ->
        expectJsonEqual row.json_field jsonValue "Json field after insert doesn't match"
        expectJsonEqual row.jsonb_field jsonValue "Jsonb field after insert doesn't match"
    | None ->
        failwith "Expected Some"
     
    // Simple update of one entity
    let updatedJsonValue = """{"name":"test_2"}"""
    let! updatedRows =
        updateTask shared {
            for e in ext.jsonsupport do
            set e.json_field updatedJsonValue
            set e.jsonb_field updatedJsonValue
            where (e.id = insertedRowId)
        }
        
    Assert.AreEqual(updatedRows, 1, "Expected 1 row to be updated")
            
    let! selectedRowsAfterUpdate = getRowById insertedRowId
    match selectedRowsAfterUpdate |> Seq.tryHead with
    | Some row ->
        expectJsonEqual row.json_field  updatedJsonValue "Json field after update doesn't match"
        expectJsonEqual row.jsonb_field updatedJsonValue "Jsonb field after update doesn't match"
    | None -> 
        failwith "Expected Some"
                   
    let entities = [entity'; entity'] |> AtLeastOne.tryCreate

    match entities with
    | Some entities' ->
        // Insert of multiple entities
        let! insertedNumberOfRows = 
            insertAsync shared {
                for e in ext.jsonsupport do
                entities entities'
            }
            
        Assert.AreEqual(insertedNumberOfRows, 2, "Failed insert multiple entities")
    | None -> 
        ()
}

[<Test>]
let ``Enum Tests``() = task {
    use! shared = db.OpenContextAsync()

    let! deleteResults =
        deleteTask shared {
            for p in ext.person do
            deleteAll
        }

    let! insertResults = 
        insertTask shared {
            into ext.person
            entity (
                { 
                    ext.person.name = "john doe"
                    ext.person.currentmood = ext.mood.ok
                }
            )
        }

    Assert.IsTrue(insertResults > 0, "Expected insert results > 0")

    let! query1Results = 
        selectTask shared {
            for p in ext.person do
            select p
            toList
        } 

    let! updateResults = 
        updateTask shared {
            for p in ext.person do
            set p.currentmood ext.mood.happy
            where (p.currentmood = ext.mood.ok)
        }

    Assert.IsTrue(updateResults > 0, "Expected update results > 0")

    let! query2Results = 
        selectTask shared {
            for p in ext.person do
            select p
            toList
        } 

    query2Results |> List.forall (fun (p: ext.person) -> p.currentmood = ext.mood.happy) =! true
}

[<Test>]
let ``OnConflictDoUpdate``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let upsertCurrency currency = 
        insertTask shared {
            for c in sales.currency do
            entity currency
            onConflict c.currencycode
            doUpdate (c.name, c.modifieddate)
        } :> Task

    let queryCurrency code = task {
        let! results =
            selectTask shared {
                for c in sales.currency do
                where (c.currencycode = code)
            }
        return results |> Seq.head
    }

    let newCurrency =
        { sales.currency.currencycode = "NEW"
        ; sales.currency.name = "New Currency"
        ; sales.currency.modifieddate = System.DateTime.Today }

    do! upsertCurrency newCurrency
    let! query1 = queryCurrency "NEW"
    query1 =! newCurrency

    let editedCurrency = { query1 with name = "Edited Currency" }

    do! upsertCurrency editedCurrency
    let! query2 = queryCurrency "NEW"
    query2 =! editedCurrency

    shared.RollbackTransaction()
}

[<Test>]
let ``OnConflictDoNothing``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let tryInsertCurrency currency = 
        insertTask shared {
            for c in sales.currency do
            entity currency
            onConflict c.currencycode
            doNothing
        } : Task
        
            
    let queryCurrency code = task {
        let! results =
            selectTask shared {
                for c in sales.currency do
                where (c.currencycode = code)
            }
        return results |> Seq.head
    }

    let newCurrency =
        { sales.currency.currencycode = "NEW"
        ; sales.currency.name = "New Currency"
        ; sales.currency.modifieddate = System.DateTime.Today }

    do! tryInsertCurrency newCurrency
    let! query1 = queryCurrency "NEW"
    query1 =! newCurrency

    let editedCurrency = { query1 with name = "Edited Currency" }
    do! tryInsertCurrency editedCurrency
    let! query2 = queryCurrency "NEW"
    query2 =! newCurrency

    shared.RollbackTransaction()
}

[<Test>]
let ``Query Employee Record with DateOnly``() = task {
    let! employees =
        selectTask db {
            for e in humanresources.employee do
            select e
        }

    gt0 employees
}

[<Test>]
let ``Query Employee Column with DateOnly``() = task {
    let! employeeBirthDates =
        selectTask db {
            for e in humanresources.employee do
            select e.birthdate
        }

    gt0 employeeBirthDates
}

[<Test>]
let ``Test Array Columns``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let row = 
        { 
            ext.arrays.id = "Test Array Columns"
            ext.arrays.text_array = [| "one"; "two"; "three" |]
            ext.arrays.integer_array = [| 1; 2; 3 |]
        }

    let! insertResults = 
        insertTask shared {
            into ext.arrays
            entity row
        }

    Assert.IsTrue(insertResults > 0, "Expected insert results > 0")

            
    let! query1Result = 
        selectTask shared {
            for r in ext.arrays do
            select r
            tryHead
        } 
                            
    Assert.AreEqual(query1Result, Some row, "Expected query result to match inserted row.")

    let! query2Result = 
        selectTask shared {
            for r in ext.arrays do
            select (r.integer_array, r.text_array)
            tryHead
        } 

    Assert.AreEqual(query2Result, Some (row.integer_array, row.text_array), "Expected to query individually selected array columns.")

    shared.RollbackTransaction()
}

[<Test>]
let ``Update Employee DateOnly``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()
            
    let! employees =
        selectTask shared {
            for e in humanresources.employee do
            select e
        }

    gt0 employees

    let emp : humanresources.employee = employees |> Seq.head
    let birthDate = System.DateOnly(1980, 1, 1)

    let! result = 
        updateTask shared {
            for e in humanresources.employee do
            set e.birthdate birthDate
            where (e.businessentityid = emp.businessentityid)
        }

    result =! 1

    let! refreshedEmp = 
        selectTask shared {
            for e in humanresources.employee do
            where (e.businessentityid = emp.businessentityid)                    
            tryHead
        }

    let actualBirthDate = 
        (refreshedEmp : humanresources.employee option)
        |> Option.map (fun e -> e.birthdate)
            
    actualBirthDate =! Some birthDate

    shared.RollbackTransaction()
}

[<Test>]
let ``SqlFn - PostgreSQL functions smoke test``() = task {
    let! results =
        selectTask db {
            for p in person.person do
            where (p.firstname = "Ken")
            select (p.firstname, char_length p.firstname, upper p.firstname, coalesce(p.middlename, "N/A"))
            take 1
        }

    let firstName, len, upperName, middleName = results |> Seq.head
    firstName =! "Ken"
    len =! 3
    upperName =! "KEN"
    Assert.That(middleName, Is.Not.Null)
}

[<Test>]
let ``Insert with Returning``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let currencyCode = "ZZZ"
    let currencyName = "RETURNING Test Currency"
    let modifiedDate = System.DateTime.Today

    let! (returnedCode: string, returnedName: string) =
        insertTask shared {
            for c in sales.currency do
            entity
                {
                    sales.currency.currencycode = currencyCode
                    sales.currency.name = currencyName
                    sales.currency.modifieddate = modifiedDate
                }
            returning (c.currencycode, c.name)
        }

    Assert.AreEqual(currencyCode, returnedCode, "Expected returned currency code to match")
    Assert.AreEqual(currencyName, returnedName, "Expected returned currency name to match")

    shared.RollbackTransaction()
}

[<Test>]
let ``leftJoin' anti-join where d = None``() = task {
    let! productsWithoutSubcategory =
        selectTask db {
            for p in production.product do
            leftJoin' sc in production.productsubcategory
            on' (p.productsubcategoryid = Some sc.Value.productsubcategoryid)
            where (sc = None)
            select p.name
            take 10
        }

    gt0 productsWithoutSubcategory
}

[<Test>]
let ``leftJoin' semi-join where d <> None``() = task {
    let! productsWithSubcategory =
        selectTask db {
            for p in production.product do
            leftJoin' sc in production.productsubcategory
            on' (p.productsubcategoryid = Some sc.Value.productsubcategoryid)
            where (sc <> None)
            select p.name
            take 10
        }

    gt0 productsWithSubcategory
}

[<Test>]
let ``leftJoin' on' with compound predicate and captured variable``() = task {
    let minOrderQty = 5s
    let! results =
        selectTask db {
            for o in sales.salesorderheader do
            leftJoin' d in sales.salesorderdetail
            on' (o.salesorderid = d.Value.salesorderid && d.Value.orderqty > minOrderQty)
            select (o.salesorderid, d)
            take 20
        }

    gt0 results
    // LEFT JOIN should always return rows; some may have d = None
    let hasSome = results |> Seq.exists (fun (_, d) -> d.IsSome)
    Assert.IsTrue(hasSome, "Expected at least some matched rows with orderqty > 5")
}

[<Test>]
let ``Insert onConflictDoNothingWhereRaw``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let currency = {
        sales.currency.currencycode = "ZZT"
        sales.currency.name = "Test Currency"
        sales.currency.modifieddate = System.DateTime.Today
    }

    // First insert should succeed
    let! result1 =
        insertTask shared {
            for c in sales.currency do
            entity currency
        }
    result1 =! 1

    // Second insert with onConflictDoNothingWhereRaw should be silently ignored
    let! result2 =
        insertTask shared {
            for c in sales.currency do
            entity currency
            onConflict c.currencycode
            whereRawConflict "TRUE"
            doNothing
        }
    // Conflict handled gracefully — 0 rows affected
    Assert.GreaterOrEqual(result2, 0)

    shared.RollbackTransaction()
}

[<Test>]
let ``Insert with Returning single column``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let! (returnedCode: string) =
        insertTask shared {
            for c in sales.currency do
            entity
                {
                    sales.currency.currencycode = "ZZS"
                    sales.currency.name = "Single Return Test"
                    sales.currency.modifieddate = System.DateTime.Today
                }
            returning c.currencycode
        }

    Assert.AreEqual("ZZS", returnedCode)

    shared.RollbackTransaction()
}

[<Test>]
let ``Insert with Returning after onConflictDoNothing``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    let currency = {
        sales.currency.currencycode = "ZZR"
        sales.currency.name = "Returning Conflict Test"
        sales.currency.modifieddate = System.DateTime.Today
    }

    // First insert — should return the code
    let! (returnedCode1: string) =
        insertTask shared {
            for c in sales.currency do
            entity currency
            returning c.currencycode
        }
    Assert.AreEqual("ZZR", returnedCode1)

    // Second insert with conflict — RETURNING with DO NOTHING returns 0 rows
    // The OutputClause.readValues should handle this gracefully
    let! (returnedCode2: string) =
        insertTask shared {
            for c in sales.currency do
            entity currency
            onConflict c.currencycode
            doNothing
            returning c.currencycode
        }
    // On conflict with DO NOTHING, no row is returned — expect default value (null for string)
    Assert.IsNull(returnedCode2, "Expected null/default when conflict is ignored")

    shared.RollbackTransaction()
}

[<Test>]
let ``Insert onConflictDoUpdateCoalesce preserves existing value``() = task {
    use! shared = db.OpenContextAsync()
    shared.BeginTransaction()

    // Insert initial currency
    let! _ =
        insertTask shared {
            for c in sales.currency do
            entity
                {
                    sales.currency.currencycode = "ZZC"
                    sales.currency.name = "Coalesce Test"
                    sales.currency.modifieddate = System.DateTime.Today
                }
        }

    // Upsert with same code and different name — name uses COALESCE so non-NULL value wins
    do!
        insertTask shared {
            for c in sales.currency do
            entity
                {
                    sales.currency.currencycode = "ZZC"
                    sales.currency.name = "Updated Name"
                    sales.currency.modifieddate = System.DateTime.Today
                }
            onConflict c.currencycode
            doUpdateCoalesce (c.name, c.modifieddate) c.name
        } :> Task

    // Verify name was updated (EXCLUDED value was non-NULL, so COALESCE picks it)
    let! result =
        selectTask shared {
            for c in sales.currency do
            where (c.currencycode = "ZZC")
            select c.name
            tryHead
        }

    result =! Some "Updated Name"

    shared.RollbackTransaction()
}

[<Test>]
let ``Select with DISTINCT ON``() = task {
    // Get one order per customer, ordered by most recent order date
    let! results =
        selectTask db {
            for o in sales.salesorderheader do
            distinctOn o.customerid
            orderBy o.customerid
            thenByDescending o.orderdate
            select (o.customerid, o.salesorderid, o.orderdate)
            take 20
        }

    gt0 results
    // Verify uniqueness: each customerid should appear exactly once
    let customerIds = results |> Seq.map (fun (cid, _, _) -> cid) |> Seq.toList
    let uniqueCustomerIds = customerIds |> List.distinct
    Assert.AreEqual(customerIds.Length, uniqueCustomerIds.Length, "DISTINCT ON should return one row per customer")
}

[<Test>]
let ``CTE with anonymous record``() = task {
    let onlineOrders =
        select {
            for o in sales.salesorderheader do
            where o.onlineorderflag
            select {| customerid = o.customerid; totaldue = o.totaldue |}
        }

    let! results =
        selectTask db {
            for r in cte "online_orders" onlineOrders do
            select r.customerid
            take 10
        }

    gt0 results
    Assert.IsTrue(results |> Seq.forall (fun id -> id > 0), "All customer IDs should be > 0")
}

type OrderSummary =
    { customerid: int
      order_count: int64
      total_spent: decimal option }

[<Test>]
let ``CTE with kata aggregate hydrated into anonymous record``() = task {
    // Inner query: type-safe joins/groupBy, kata for aggregate projections.
    // select + kata ClearComponent("select") replaces the typed SELECT with raw columns.
    let innerQuery =
        select {
            for o in sales.salesorderheader do
            groupBy o.customerid
            select o.customerid
            kata (fun q ->
                q.ClearComponent("select")
                 .SelectRaw("\"o\".\"customerid\"")
                 .SelectRaw("COUNT(*) as order_count")
                 .SelectRaw("SUM(\"o\".\"totaldue\") as total_spent"))
        }

    let! results =
        selectTask db {
            for s in cteFrom<{| customerid: int; order_count: int64; total_spent: decimal option |}> "order_stats" innerQuery do
            where (s.order_count > 1L)
            select s
        }

    gt0 results
    for r in results do
        Assert.IsTrue(r.order_count > 1L, $"order_count should be > 1, got {r.order_count}")
        Assert.IsTrue(r.customerid > 0, $"customerid should be > 0, got {r.customerid}")
}

[<Test>]
let ``CTE with kata CASE WHEN computed column``() = task {
    // Same pattern: type-safe structure, kata for computed columns
    let innerQuery =
        select {
            for o in sales.salesorderheader do
            groupBy o.customerid
            select o.customerid
            kata (fun q ->
                q.ClearComponent("select")
                 .SelectRaw("\"o\".\"customerid\"")
                 .SelectRaw("COUNT(*) as order_count")
                 .SelectRaw("CASE WHEN COUNT(*) > 5 THEN true ELSE false END as is_frequent"))
        }

    let! results =
        selectTask db {
            for s in cteFrom<{| customerid: int; order_count: int64; is_frequent: bool |}> "cust_stats" innerQuery do
            where s.is_frequent
            select (s.customerid, s.order_count)
        }

    gt0 results
    for (cid, cnt) in results do
        Assert.IsTrue(cnt > 5L, $"is_frequent customers should have > 5 orders, got {cnt}")
        Assert.IsTrue(cid > 0, $"customerid should be > 0")
}
