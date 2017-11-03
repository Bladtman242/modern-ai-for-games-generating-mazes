module Lattice
open Block

type LBlock = { b: unit }

type Lat = { b: unit }

type LatPos = { b: unit }

let exits (lb : LBlock) : LatPos list = [{b = ()}]

let neighbours (lat : Lat) (pos : LatPos) : LBlock list = [{b = ()}]

let fitDef (lat : Lat) (pos : LatPos) : ExitVect = Block.exits Block.create

let fit (vect : ExitVect) (b : Block) : LBlock option = Some {b = ()}

let addBlock (lat : Lat) (lb : LBlock) : Lat = {b = () }
