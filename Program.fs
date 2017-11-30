
open Block
open Evolution
open Graph


type Genome = (Graph<int*int>*Graph<int*int>) list


[<EntryPoint>]
let main argv = 
    System.Console.OutputEncoding <- System.Text.Encoding.UTF8

    // Constants
    let rnd = new System.Random()
    let seed = Graph.empty |> Graph.addEdge (0,0) (0,1)
    let emptyRule = (Graph.empty, Graph.empty)
    
    // Helper functions
    let rec applyFirst rules g =
        match rules with
        | (l,r)::rs -> 
            let res = StructureGraph.applyRule l r g
            if res = None then applyFirst rs g else res
        | [] -> None
    let rec applyRules rules g i =
        if i = Constants.ApplyLimit then g
        else
            let res = applyFirst rules g
            match res with
            | None -> g
            | Some(r) -> applyRules rules r (i+1)
    let replaceAt p f =
        List.mapi (fun i x -> if i = p then f x else x)
    let findPossible g ns : (((int*int) * (int*int) list) list) =
        ns
        |> List.map (fun ((ax,ay) as a) -> 
            let r = StructureGraph.nodeNeighbourhood a g 
                    |> Neighbourhood.mapWithDir (fun n d -> 
                        if n <> None then None 
                        else match d with 
                             | 0 -> Some(ax,ay+1) 
                             | 1 -> Some(ax+1,ay) 
                             | 2 -> Some(ax,ay-1)
                             | 3 -> Some(ax-1,ay)
                             | _ -> None)
                    |> Neighbourhood.toList
                    |> List.choose id
            (a,r))
        |> List.where (fun (a,r) -> r.Length > 0)
    let flip f y x = f x y
        
    let addEdge g =
        let ns = Graph.nodes g |> Set.toList
        if ns.Length = 0 then Graph.addEdge (0,0) (0,1) g
        else
            let allPossible = findPossible g ns
            let (a,possible) = allPossible.Item (rnd.Next allPossible.Length)
            let b = possible.Item (rnd.Next possible.Length)
            Graph.addEdge a b g
    let addToLeft rule =
        (addEdge <| fst rule, snd rule)
    let addToRight rule =
        (fst rule, addEdge <| snd rule)
    let remEdge g =
        let ns = Graph.nodes g |> Set.toList
        if ns.Length = 0 then Graph.addEdge (0,0) (0,1) g
        else
            let allPossible = ns |> List.map (fun n -> (n, StructureGraph.nodeNeighbourhood n g
                                                           |> Neighbourhood.toList |> List.choose id))
            let (a,possible) = allPossible.Item (rnd.Next allPossible.Length)
            let b = possible.Item (rnd.Next possible.Length)
            Graph.removeEdge a b g
    let remFrLeft rule =
        (addEdge <| fst rule, snd rule)
    let remFrRight rule =
        (fst rule, addEdge <| snd rule)
    
    
    // Parameters for the algorithm
    let initPop : Population<Genome> = List.init Constants.PopulationSize (fun _ -> [])
    let muts : (Mutation<Genome>*int) list = [
        ((fun i -> i), 0);                                          // Nothing
        ((fun i ->                                                  // Add new rule if missing         
             if List.contains emptyRule i then i else emptyRule::i), 1);                               
        ((fun i -> replaceAt (rnd.Next i.Length) addToLeft i), 2);  // Add edge to a left
        ((fun i -> replaceAt (rnd.Next i.Length) addToRight i), 3); // Add edge to a right             
        ((fun i -> replaceAt (rnd.Next i.Length) remFrLeft i), 2);  // Remove edge from a left
        ((fun i -> replaceAt (rnd.Next i.Length) remFrRight i), 3); // Remove edge from a right
    ]
    let eval = fun (rs) -> 
        let g = applyRules rs seed 0
        let size = Graph.nodes g |> Set.count |> double
        -abs(30.0 - size) - (float (pitfalls g))/size - (Graph.avgDegree g)
        
    let sel = fun n (i, s) -> i < n/4
    let breed = fun (a,b) -> a
    
    // Run the algorithm
    let (_,res) = Evolution.train rnd muts eval sel breed initPop
    let output = res 
                 |> List.sortByDescending eval 
                 |> List.take Constants.PrintTopResults
                 |> List.map (fun g -> applyRules g seed 0) 
                 |> List.map (StructureGraph.toLat (StructureGraph.picker rnd))
                 
    for lat in output do
        Lattice.print lat; System.Console.WriteLine "-------------------------"
    
    0
