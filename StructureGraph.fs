module StructureGraph
open Graph

let rnd = System.Random ()
let blocks = List.init 100 (fun _ -> Block.createRandom rnd)

let findRot (l: Lattice.Lat) (p : Lattice.Pos) =
    List.find (fun b -> Option.isSome <| Lattice.placeBlock b p l) blocks

let toLat (g: (int*int) Graph) : Lattice.Lat =
    let nodeSet = Graph.nodes g
    Set.fold
        (fun l p -> Option.bind (fun x -> Lattice.placeBlock (findRot x p) p x) l)
        (Some Lattice.emptyLat)
        nodeSet
 |> Option.get
