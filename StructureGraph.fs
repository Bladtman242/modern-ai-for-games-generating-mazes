module StructureGraph
open Graph
open Neighbourhood

type LatGen = (int*int) Graph -> Lattice.Lat
type BlockPicker = Lattice.Pos Set ->
                    Lattice.Pos ->
                    Lattice.Lat ->
                    Lattice.LBlock

let private rnd = System.Random ()
let private blocks = List.init 100000 (fun _ -> Block.createRandom rnd)

let doesFitEdges (edges : Lattice.Pos Set) (lb : Lattice.LBlock) : bool = (Lattice.exits lb |> Set.ofList) = edges

let rndPicker : BlockPicker = fun conNeighbours p l ->
    let blockExitFits = List.map (fun b -> Lattice.fits (Lattice.fitDef l p) l b) blocks
    let lBlocks = List.zip blocks blockExitFits
               |> List.filter (snd >> List.isEmpty >> not)
               |> List.map (fun (b,os) -> List.map (fun o -> Lattice.LBlock b o p) os)
               |> List.concat
    let blockEdgeFits = List.filter (doesFitEdges conNeighbours) lBlocks
    List.head blockEdgeFits
    
let picker rnd neighbors (x,y) lat : Lattice.LBlock = 
    let pos = (x,y)
    let neighborhood = Lattice.neighbourhoodLBlocks lat pos
    let offset d =
        match d with
        | 0 -> (x,y+1)
        | 1 -> (x+1,y)
        | 2 -> (x,y-1)
        | 3 -> (x-1,y)
        | _ -> raise (System.Exception "This shouldn't happen")
    let exit (d:int) =
        match d with
        | 0 -> Block.south
        | 1 -> Block.west
        | 2 -> Block.north
        | 3 -> Block.east
        | _ -> raise (System.Exception "This shouldn't happen")
    let exithood = neighborhood |> Neighbourhood.map2 (fun b r -> 
        if b.IsSome 
        then if Set.contains (Lattice.pos b.Value) neighbors 
             then printfn "A %A" r
                  Some(Lattice.exitVect b.Value |> exit r |> Block.reverseExitVect)
             else printfn "B %A" r
                  Some(Block.emptyExitVect)
        else if Set.contains (offset r) neighbors
             then printfn "C %A" r
                  Some(Block.randomExitVect rnd 0.5)
             else printfn "D %A" r
                  None
    ) 
    printfn "%A" exithood
    Lattice.LBlock (Block.create rnd exithood) 0 pos
    

let toLat : BlockPicker -> LatGen = fun f g ->
    let pickBlock l n =
        let connectedNeighbours = Graph.adjacentTo n g
        Lattice.addBlock (f connectedNeighbours n l) l

    let nodeSet = Graph.nodes g
    Set.fold pickBlock Lattice.emptyLat nodeSet
