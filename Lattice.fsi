module Lattice
open Block

type LBlock

type Lat
type LatPos

val exits : (LBlock -> LatPos list)
val neighbours : (Lat -> LatPos -> LBlock list)
val fitDef : (Lat -> LatPos -> ExitVect)
val fit : (ExitVect -> Block -> LBlock option)
val addBlock : (Lat -> LBlock -> Lat)

// vim: set sw=4 ts=4 et:
