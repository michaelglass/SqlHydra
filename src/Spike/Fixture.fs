module Spike.Fixture

open SqlHydra.Query

module sales =
    [<CLIMutable>]
    type invoice =
        { id: int
          code: string
          price: decimal
          tax: decimal }

    and [<CLIMutable>] invoice_write =
        { code: string
          price: decimal }
        interface SqlHydra.IWriteOf<invoice>

let rows = table<sales.invoice>
let row : sales.invoice_write = { code = "a"; price = 10m }
