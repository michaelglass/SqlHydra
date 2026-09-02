module Probe
open SqlHydra
// The degenerate case: every column read-only leaves nothing for the write record to hold.
type all_generated_write =
    { }
    interface IWriteOf<obj>
