module StructureGraph
open Graph
open Neighbourhood
open OptionExtensions

type StructGraph = (int*int) Graph

type LatGen = StructGraph -> Lattice.Lat
type BlockPicker = Lattice.Pos Set ->
                    Lattice.Pos ->
                    Lattice.Lat ->
                    Lattice.LBlock

let private rnd = System.Random ()
let private blocks = List.init 100000 (fun _ -> Block.createRandom rnd)

let doesFitEdges (edges : Lattice.Pos Set) (lb : Lattice.LBlock) : bool = (Lattice.exits lb |> Set.ofList) = edges

let private rndFit : BlockPicker = fun conNeighbours p l ->
    let blockExitFits = List.map (fun b -> Lattice.fits (Lattice.fitDef l p) l b) blocks
    let lBlocks = List.zip blocks blockExitFits
               |> List.filter (snd >> List.isEmpty >> not)
               |> List.map (fun (b,os) -> List.map (fun o -> Lattice.LBlock b o p) os)
               |> List.concat
    let blockEdgeFits = List.filter (doesFitEdges conNeighbours) lBlocks
    List.head blockEdgeFits

let toLat : BlockPicker -> LatGen = fun f g ->
    let pickBlock l n =
        let connectedNeighbours = Graph.adjacentTo n g
        Lattice.addBlock (f connectedNeighbours n l) l

    let nodeSet = Graph.nodes g
    Set.fold pickBlock Lattice.emptyLat nodeSet

let toLatWithRandomBlocks : LatGen = toLat rndFit

let private nodeNeighbourhood (n : int*int) (g : StructGraph) : (int*int) option Neighbourhood =
    let neighBourOrNone n' = if Graph.areAdjacent n n' g
                               then Some n'
                               else None
    Lattice.neighbourhood n
 |> Neighbourhood.map neighBourOrNone

let doesRuleMatch (rule : StructGraph) (p : int*int) (g : StructGraph) : bool =
    let rec doesMatch (rot : int) (rp : int*int) (p : int*int) : bool =
        let rn = Neighbourhood.rotate rot <| nodeNeighbourhood rp rule
        let gn = nodeNeighbourhood p g
        let hasSameEdges = Neighbourhood.map Option.isSome gn = Neighbourhood.map Option.isSome rn
        printfn "%A" rn
        printfn "%A" gn
        if not hasSameEdges then printfn "3"; false
        else printfn "4"; Neighbourhood.map2 (fun pOpt dir -> Option.map (fun p -> doesMatch rot p (Option.get (Neighbourhood.get dir gn))) pOpt)
                                             rn
          |> Neighbourhood.map (Option.getOrElse true)
          |> Neighbourhood.toList
          |> List.forall id

    let ruleRoot = Graph.nodes rule |> Set.toList |> List.head
    let adjs = Graph.adjacentTo p g
    //early termination if the graph node has lower degree than the rule node
    if Set.count adjs < Set.count (Graph.adjacentTo ruleRoot rule)
    then printfn "1"; false
    else printfn "2"; Set.exists (fun n -> doesMatch 0 ruleRoot n) (Graph.nodes g)

