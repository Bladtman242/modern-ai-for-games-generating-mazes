module Lattice
open Block
open Neighbourhood

type LBlock

type Lat
type LatPos

val emptyLat : Lat

// list of lattice positions the LBlock has exits to, regardless of whether or
// not there are blocks in those positions
val exits : (LBlock -> LatPos list)

// List of LBlocks in the positions surrounding the given posision
val neighbours : (Lat -> LatPos -> LBlock list)
val fitDef : (Lat -> LatPos -> ExitVect)
val fits : (Neighbourhood<ExitVect option> -> bool list -> Lat -> Block -> int list)
val addBlock : (Lat -> LBlock -> Lat)

// vim: set sw=4 ts=4 et:
