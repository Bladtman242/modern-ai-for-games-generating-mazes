module StructureGraph
open Graph

type LatGen = (int*int) Graph -> Lattice.Lat
type BlockPicker = Lattice.Pos -> Lattice.Lat -> Lattice.LBlock

let private rnd = System.Random ()
let private blocks = List.init 200 (fun _ -> Block.createRandom rnd)

let private rndFit (p : Lattice.Pos) (l: Lattice.Lat) =
    let fits = List.filter (fun b -> Option.isSome <| Lattice.placeBlock b p l) blocks
    let b = List.item (rnd.Next (List.length fits)) fits
    Lattice.LBlock b (List.head <| Lattice.fits (Lattice.fitDef l p) l b) p

let toLat : BlockPicker -> LatGen = fun f g ->
    let nodeSet = Graph.nodes g
    Set.fold (fun l n -> Lattice.addBlock (f n l) l) Lattice.emptyLat nodeSet

let toLatWithRandomBlocks : LatGen = toLat rndFit
