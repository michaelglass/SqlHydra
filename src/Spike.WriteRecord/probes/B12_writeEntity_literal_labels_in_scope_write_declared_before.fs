module Probe
open SqlHydra
open SqlHydra.Query
module local =
    type t_write =
        { price: decimal }
        interface IWriteOf<t>
    and t =
        { [<ReadOnlyColumn>] id: int
          price: decimal }
open local
let ts = table<t>
let q () =
    insert {
        into ts
        writeEntity { price = 10m }
    }
