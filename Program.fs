﻿
open Block
open Evolution
open Graph
open StructureGraph


[<EntryPoint>]
let main argv = 
    System.Console.OutputEncoding <- System.Text.Encoding.UTF8
    
    let evals : (double*(StructGraph->double)) list = [
        ( 1.0, fun g -> 
            let size = Graph.nodes g |> Set.count |> double
            -abs(30.0-size)
        );
        (-1.0, fun g -> 
            let size = Graph.nodes g |> Set.count |> double
            double (pitfalls g)/size
        );
        //(-1.0, fun g -> Graph.avgDegree g);
        (-1.0, fun g ->
            let (width,height) = Graph.boundingBoxSize g
            let avgDist = double <| Graph.avgDistance g
            abs (avgDist - sqrt (double (width*height)))
        );
        (-1.0, fun g ->
            let (numSacs,sacLen) = Graph.culDeSacsCountLength g
            let size = Graph.nodes g |> Set.count |> double
            abs (0.05 - (double numSacs / size)) + abs (3.0 - size)
        )]
   
    let output = GraphEvolve.run evals
                 
    for lat in output do
        Lattice.print lat; System.Console.WriteLine "-------------------------"
    
    0
