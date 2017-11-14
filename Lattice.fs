module Lattice
open Block
open Neighbourhood
open OptionExtensions

type Pos = int * int

// An LBlock consists of the "original" block (defining the layout/maze of an
// area), a position in the lattice (the full maze), and an orientation
// represented by the number of 90 degree clockwise rotations of the template
// block
type LBlock = { template : Block; position : Pos; orientation : int}

type Lat = { lat : Map<Pos,LBlock> }

let LBlock b o p = { template = b; position = p; orientation = o}

let exitVect ({template = b; position = _; orientation = r} : LBlock) : ExitVect =
    Block.rotate r (Block.exits b)

let neighbourhood ((x,y) : Pos) : Neighbourhood<Pos> =
    {
        north = (x,y-1);
        east =  (x+1,y);
        south = (x,y+1);
        west =  (x-1,y)
    }

let emptyLat = { lat = Map.empty }

let exits (lb : LBlock) : Pos list =
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


let neighbourhoodLBlocks (lat : Lat) (pos : Pos) : Neighbourhood<LBlock option> =
    let neighbourPositions = neighbourhood pos
    let lBlocks = List.map (fun k -> Map.tryFind k lat.lat) (neighbourPositions.toList)
    { north = List.item 0 lBlocks;
      east = List.item 1 lBlocks;
      south = List.item 2 lBlocks;
      west = List.item 3 lBlocks
    }

let neighbours (lat : Lat) (pos : Pos) : LBlock list =
    let neighbourPositions = neighbourhoodLBlocks lat pos
    List.collect Option.toList neighbourPositions.toList

let fitDef (lat : Lat) (pos : Pos) : Neighbourhood<ExitVect option> =
    // Get the neighbouring blocks' vectors, with proper rotation
    let neighbourExits = neighbourhoodLBlocks lat pos
                      |> Neighbourhood.map (Option.map exitVect)
    // Pick the relevant sides from the neighbouring vectors, and return
    {
        north = Option.map Block.south neighbourExits.north;
        east = Option.map Block.west neighbourExits.east;
        south = Option.map Block.north neighbourExits.south;
        west = Option.map Block.east neighbourExits.west
    }

let fits (neigbourhood : Neighbourhood<ExitVect option>) (lat : Lat) (b : Block) : int list =
    Block.fit neigbourhood b

let addBlock (lb : LBlock) (lat: Lat) : Lat =
    let newMap = Map.add lb.position lb lat.lat
    { lat = newMap }

let placeBlock (block: Block) (pos : Pos) (lat : Lat) : Lat option =
    let neighbourVectors = fitDef lat pos
    let possibleFits = Block.fit neighbourVectors block
    if List.isEmpty possibleFits then None
    else let lb = { template = block;
                    position = pos;
                    orientation = List.head possibleFits }
         Some <| addBlock lb lat

let lBlockAt (pos: Pos) (lat: Lat) : LBlock option =
    Map.tryFind pos lat.lat

let toStrings (lat : Lat) : string list =
    let size = Constants.BlockSize * 2 + 1
    let emptyBlock =
        let row = String.init size (fun _ -> " ")
        List.init size (fun _ -> row)
    let getAsString (p : int * int) : string list =
        lBlockAt p lat
     |> Option.map (fun lb -> Block.print lb.template lb.orientation)
     |> Option.getOrElse emptyBlock

    let row (y: int) ((xMin,xMax) : int*int) : string list =
        let blockStrings : string list list =
            [ for x in [xMin..xMax] do
                yield getAsString (x,y)
            ]
        List.reduce (List.map2 (+)) blockStrings

    let coords : (int*int) list = Map.toList lat.lat |> List.map fst
    let minX = List.minBy fst coords |> fst
    let maxX = List.maxBy fst coords |> fst
    let minY = List.minBy snd coords |> snd
    let maxY = List.maxBy snd coords |> snd
    [ for y in [minY..maxY] do
        yield! row y (minX,maxX) ]

let print (lat: Lat) = List.iter (printfn "%s") (toStrings lat)

// vim: set sw=4 ts=4 et:
