module Lattice
open Block
open Neighbourhood

type Pos = int * int

type LatPos = { pos: Pos }

// An LBlock consists of the "original" block (defining the layout/maze of an
// area), a position in the lattice (the full maze), and an orientation
// represented by the number of 90 degree clockwise rotations of the template
// block
type LBlock = { template : Block; position : LatPos; orientation : int}

type Lat = { lat : Map<LatPos,LBlock> }

let neighbourhood (p : LatPos) : Neighbourhood<LatPos> =
    let (x,y) = p.pos
    {
        north = { pos = (x,y+1) };
        east =  { pos = (x+1,y) };
        south = { pos = (x,y-1) };
        west =  { pos = (x-1,y) }
    }

let emptyLat = { lat = Map.empty }

let exits (lb : LBlock) : LatPos list =
    let { template = b; position = lpos; orientation = o } = lb
    let lbExits = Block.rotate o (Block.exits b)
    let neighbourhood = neighbourhood lpos
    let neighbouringPositions = [
        neighbourhood.north, Block.north lbExits;
        neighbourhood.east, Block.east lbExits;
        neighbourhood.south, Block.south lbExits;
        neighbourhood.west, Block.west lbExits
    ]
    List.filter (Block.hasExit << snd) neighbouringPositions
 |> List.map fst

let neighbourhoodLBlocks (lat : Lat) (pos : LatPos) : Neighbourhood<LBlock option> =
    let neighbourPositions = neighbourhood pos
    let lBlocks = List.map (fun k -> Map.tryFind k lat.lat) (neighbourPositions.toList)
    { north = List.item 0 lBlocks;
      east = List.item 1 lBlocks;
      south = List.item 2 lBlocks;
      west = List.item 3 lBlocks
    }

let neighbours (lat : Lat) (pos : LatPos) : LBlock list =
    let neighbourPositions = neighbourhoodLBlocks lat pos
    List.collect Option.toList neighbourPositions.toList

let fitDef (lat : Lat) (pos : LatPos) : ExitVect =
    let rotate {template = b; position = _; orientation = o} =
            Block.rotate o (Block.exits b)
    List.map rotate (neighbours lat pos)
 |> Block.concat


let fits (neigbourhood : Neighbourhood<ExitVect option>) (area : bool list) (lat : Lat) (b : Block) : int list =
    Block.fit neigbourhood b

//let place (block: Block) (pos : LatPos) (lat : Lat) Lat option =
//    let neighbourhood = neighbourhoodLBlocks lat pos

let addBlock (lat : Lat) (lb : LBlock) : Lat =
    let newMap = Map.add lb.position lb lat.lat
    { lat = newMap }

// vim: set sw=4 ts=4 et:
