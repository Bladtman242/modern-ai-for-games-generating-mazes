
open Block
open Evolution
open Graph
open StructureGraph
open OptionExtensions


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
                             else let pits = pitfalls g
                                  30.0 * double pits/size
                pitFit
            );
            (-1.0,
                let (width,height) = Graph.boundingBoxSize g
                let fit = if 0.0 = size then 0.0
                          else let medianDist = double <| Graph.medianDistance g
                               medianDist / (1.0 + sqrt (double (width*height)))
                fit
            );
            (-1.0,
                let culFit = if 0.0 = size then 0.0
                             else 50.0 * abs (0.05 - (double numSacs / size))
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
            let (numTrees,treeLength) = Graph.treesCountLength g
            //let (numSacs,sacLen) = Graph.culDeSacsCountLengts g
            [
            ( -1.0,
                (abs (30.0-size)) ** 1.3
            );
            ( 1.0,
                double <| numTrees
            );
            ( 1.0,
                let (numSacs,sacLen) = Graph.culDeSacsCountLengts g
                let medianSacLenOpt = List.tryItem (numSacs/2) sacLen
                Option.map (fun medianSac -> double treeLength / double medianSac) medianSacLenOpt
             |> Option.map (fun ratio -> log ratio / log 1.02)
             |> Option.getOrElse 0.0
            );
            //( 1.0,
            //    double <| treeLength
            //);
            ( -1.0,
                12.0 ** abs (2.5 - Graph.avgDegree g)
            );
            ]

    let output = GraphEvolve.run dungeonCrawlerEval
                 
    for lat in output do
        Lattice.print lat; System.Console.WriteLine "-------------------------"
    
    0
