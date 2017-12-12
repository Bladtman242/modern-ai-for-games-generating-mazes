
open Block
open Evolution
open Graph
open StructureGraph


[<EntryPoint>]
let main argv = 
    System.Console.OutputEncoding <- System.Text.Encoding.UTF8
    
    //few but some pitfalls
    //few cul de sacs
    //small avg travel
    let copsAndRobbersEval : StructGraph -> (double*double) list =
        fun g ->
            let size = Graph.nodes g |> Set.count |> double
            let (numSacs,sacLen) = Graph.culDeSacsCountLengts g
            [
            ( -1.0,
                (abs (30.0-size)) ** 1.3
            );
            (-1.0,
                let pitFit = if 0.0 = size then 0.0
                             else 30.0 * double (pitfalls g)/size
                pitFit
            );
            (-1.0,
                let (width,height) = Graph.boundingBoxSize g
                let medianDist = double <| Graph.medianDistance g
                let fit = if 0.0 = size then 0.0
                          else medianDist / (1.0 + sqrt (double (width*height)))
                fit
            );
            (-1.0,
                let culFit = if 0.0 = size then 0.0
                             else abs (0.05 - (double numSacs / size))
                culFit
            );
            (-1.0,
                let culFit = if 0 = numSacs then 0.0
                             else (double <| abs (3 - (List.head sacLen))) ** 4.0
                culFit
            );
            ]

    //Rich in cul de sacs
    //Rich in trees
    //low degree
    let dungeonCrawlerEval : StructGraph -> (double*double) list =
        fun g ->
            let size = Graph.nodes g |> Set.count |> double
            [
            ( 1.0,
                -abs(30.0-size)
            );
            ( 1.0,
                let (numTrees,treeLength) = Graph.treesCountLength g
                double <| numTrees + treeLength
            );
            ( 1.0,
                Graph.avgDegree g
            );
            ]

    let output = GraphEvolve.run copsAndRobbersEval
                 
    for lat in output do
        Lattice.print lat; System.Console.WriteLine "-------------------------"
    
    0
