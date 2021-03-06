module Lattice
open Block
open Neighbourhood

type LBlock

type Lat
type Pos = int*int

val emptyLat : Lat

// Prints the lattice to the console
val print : Lat -> unit
val toStrings : Lat -> string list

// list of lattice positions the LBlock has exits to, regardless of whether or
// not there are blocks in those positions
val exits : (LBlock -> Pos list)
val LBlock : Block -> int -> Pos -> LBlock
val exitVect : (LBlock -> Block.ExitVect)

// List of LBlocks in the positions surrounding the given posision
val neighbourhood : Pos -> Pos Neighbourhood
val neighbours : (Lat -> Pos -> LBlock list)
val neighbourhoodLBlocks : (Lat -> Pos -> (LBlock option) Neighbourhood)
val fitDef : (Lat -> Pos -> Neighbourhood<ExitVect option>)
val fits : (Neighbourhood<ExitVect option> -> Lat -> Block -> int list)
val placeBlock : (Block -> Pos -> Lat -> Lat option)
val addBlock : (LBlock -> Lat -> Lat)
val pos : LBlock -> Pos
val block : LBlock -> Block

