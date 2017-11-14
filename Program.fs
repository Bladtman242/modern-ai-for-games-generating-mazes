
open Block

[<EntryPoint>]
let main argv = 
    System.Console.OutputEncoding <- System.Text.Encoding.UTF8


    let g1 = Graph.addEdge (0,0) (1,0) Graph.empty
          |> Graph.addEdge (0,0) (0,1)
          |> Graph.addEdge (0,1) (1,1)
          |> Graph.addEdge (0,1) (0,2)
          |> Graph.addEdge (0,2) (1,2)
          |> Graph.addEdge (2,1) (1,1)

    Lattice.print <| StructureGraph.toLat g1
    0
