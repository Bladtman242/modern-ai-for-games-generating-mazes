module StructureGraph
open Graph

let toLat (g: (int*int) Graph) : Lattice.Lat =
    let rnd = System.Random 1
    let nodeSet = Graph.nodes g
    Set.fold
        (fun l p -> Option.bind (Lattice.placeBlock (Block.createRandom rnd) p) l)
        (Some Lattice.emptyLat)
        nodeSet
 |> Option.get
