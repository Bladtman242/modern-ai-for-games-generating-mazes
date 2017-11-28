module StructureGraph

open Graph
open Neighbourhood
open OptionExtensions
open SeqExtensions

type StructGraph = (int*int) Graph

type Edge = (int*int)*(int*int)

type RuleMatch =
    private {
        //Rotation to go from rule space to matched graph space
        rotation : int
        //The point in rule space to rotate around
        rulePoint : int * int
        //The delta translation to from rule space to matched graph space
        pointDelta : int * int
    }

type LatGen = StructGraph -> Lattice.Lat
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
    let exithood = neighborhood |> Neighbourhood.mapWithDir (fun b r -> 
        if b.IsSome 
        then if Set.contains (Lattice.pos b.Value) neighbors 
             then Some(Lattice.exitVect b.Value |> exit r |> Block.reverseExitVect)
             else Some(Block.emptyExitVect)
        else if Set.contains (offset r) neighbors
             then Some(Block.randomExitVect rnd 0.5)
             else None
    ) 
    Lattice.LBlock (Block.create rnd exithood) 0 pos
    

let toLat : BlockPicker -> LatGen = fun f g ->
    let pickBlock l n =
        let connectedNeighbours = Graph.adjacentTo n g
        Lattice.addBlock (f connectedNeighbours n l) l

    let nodeSet = Graph.nodes g
    Set.fold pickBlock Lattice.emptyLat nodeSet

let private nodeNeighbourhood (n : int*int) (g : StructGraph) : (int*int) option Neighbourhood =
    let neighBourOrNone n' = if Graph.areAdjacent n n' g
                               then Some n'
                               else None
    Lattice.neighbourhood n
 |> Neighbourhood.map neighBourOrNone

let matchRule (rule : StructGraph) (g : StructGraph) : RuleMatch option =
    //logical implication. a implies b.
    let (=>) a b = not a || b
    let rec doesMatch (visited : (int*int) Set) (rot : int) (rp : int*int) (p : int*int) : bool =
        if Set.contains rp visited
        then true
        else let visited' = Set.add rp visited
             let rn = Neighbourhood.rotate rot <| nodeNeighbourhood rp rule
             let gn = nodeNeighbourhood p g
             let gnHasRnEdges = List.map2 (=>) (List.map Option.isSome rn.toList) (List.map Option.isSome gn.toList)
                             |> List.forall id
             if not gnHasRnEdges then false
             else let recMatches : (bool option) Neighbourhood = Neighbourhood.mapWithDir
                                                                    (fun pOpt dir -> Option.map (fun p -> doesMatch visited' rot p (Option.get (Neighbourhood.get dir gn))) pOpt)
                                                                    rn
                  Neighbourhood.map (Option.getOrElse true) recMatches
               |> Neighbourhood.toList
               |> List.forall id

    let ruleRootOpt = Graph.nodes rule |> Set.toList |> List.tryHead
    if Option.isNone ruleRootOpt then None
    else let (rx,ry) as ruleRoot = Option.get ruleRootOpt
         let nodes = Graph.nodes g
         let rsAndNodes = Seq.zip (Seq.infinite 0) nodes
                       |> Seq.append (Seq.zip (Seq.infinite 1) nodes)
                       |> Seq.append (Seq.zip (Seq.infinite 2) nodes)
                       |> Seq.append (Seq.zip (Seq.infinite 3) nodes)
         Seq.filter (fun (r,n) -> doesMatch Set.empty r ruleRoot n) rsAndNodes
      |> Seq.tryHead
      |> Option.map (fun (r,(x,y)) -> {rotation = r; rulePoint = ruleRoot; pointDelta = (x-rx, y-ry)})
     
 // takes left- and right-hand side of a grammar rule, and returns a tuple (add
 // set, remove set) of edge sets Note: the returned change-sets are in the
 // same space as the given rule, so if a change-set in mathed graph space is
 // desired, the rule should be in matched graph space. (see ruleInMatchSpace)
let ruleDelta (leftHand : StructGraph) (rightHand : StructGraph) : (Edge Set * Edge Set) =
    let leftEdges = Graph.nodes leftHand
                 |> Seq.collect (fun n -> Graph.adjacentTo n leftHand |> Set.map (fun n' -> (n, n')))
                 |> Set.ofSeq
    let rightEdges = Graph.nodes rightHand
                  |> Seq.collect (fun n -> Graph.adjacentTo n rightHand |> Set.map (fun n' -> (n, n')))
                  |> Set.ofSeq
    let uniqueToLeft = Set.difference leftEdges rightEdges
    let uniqueToRight = Set.difference rightEdges leftEdges
    (uniqueToRight,uniqueToLeft)

let rec rotatePoint (r : int) ((x,y) as p : int*int) : int*int =
    if r = 0 then p
    else rotatePoint (r-1) (-y,x)

let translatePoint ((dx,dy) : int*int) ((x,y) : int*int) : int*int =
    (x+dx, y+dy)

let transformPoint (rm: RuleMatch) ((x,y) : int*int) : int*int =
    let (cx,cy) = rm.rulePoint
    let delta = (x-cx,y-cy)
    let (irx,iry) = rotatePoint rm.rotation delta
    translatePoint (cx,cy) (irx, iry)
 |> translatePoint rm.pointDelta


//applies a rule diff (addset*removeset) to a graph
let applyDelta ((addSet,removeSet) : (Edge Set) * (Edge Set)) (rm : RuleMatch) (g : StructGraph) : StructGraph =
    let transformEdge (p1,p2) = (transformPoint rm p1, transformPoint rm p2)
    let addSet' = Set.map transformEdge addSet
    let removeSet' = Set.map transformEdge removeSet
    let graphWithAdded = Set.fold (fun g (p1,p2) -> Graph.addEdge p1 p2 g) g addSet'
    Set.fold (fun g (p1,p2) -> Graph.removeEdge p1 p2 g) graphWithAdded removeSet'

//maps the rule to the graph space, and applies it to the graph
let applyRule (leftHand : StructGraph) (rightHand : StructGraph) (g : StructGraph) : StructGraph option =
    match matchRule leftHand g with
    | None -> None
    | Some matching ->
        let delta = ruleDelta leftHand rightHand
        Some <| applyDelta delta matching g

