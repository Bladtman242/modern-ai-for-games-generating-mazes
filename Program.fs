
open Block

[<EntryPoint>]
let main argv = 
    System.Console.OutputEncoding <- System.Text.Encoding.UTF8

    let rnd = new System.Random()
    let g1 = Graph.empty
          |> Graph.addEdge (0,0) (1,0) 
          |> Graph.addEdge (1,0) (2,0)
          |> Graph.addEdge (2,0) (2,1)
          |> Graph.addEdge (2,1) (1,1)
          |> Graph.addEdge (1,1) (0,1)
          |> Graph.addEdge (0,1) (0,0)
          |> Graph.addEdge (2,1) (3,1)
          |> Graph.addEdge (3,1) (4,1)
          |> Graph.addEdge (4,1) (4,0)

    Lattice.print <| StructureGraph.toLat (StructureGraph.picker rnd) g1
    0
