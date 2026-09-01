// Which CLR types can carry a unit of measure at all?
module Probe
open Spike.Support
open System

// numerics: fine
let a: int<ro> = 1<ro>
let b: int64<ro> = 1L<ro>
let c: decimal<ro> = 1m<ro>
let d: float<ro> = 1.0<ro>
let e: float32<ro> = 1.0f<ro>
let f: int16<ro> = 1s<ro>
let g: byte<ro> = 1uy<ro>
let h: uint32<ro> = 1u<ro>

// everything else: no measure exists
let i: string<ro> = ""
let j: bool<ro> = false
let k: DateTime<ro> = DateTime.Now
let l: Guid<ro> = Guid.Empty
let m: DateTimeOffset<ro> = DateTimeOffset.Now
