module GraphEvolve

open Graph
open StructureGraph
open Evolution

type Genome = (Graph<int*int>*Graph<int*int>) list


// Constants
let private rnd : System.Random = new System.Random();
let private seed = Graph.empty |> Graph.addEdge (0,0) (0,1)
let private emptyRule = (Graph.empty, Graph.empty)

// Helper functions
let rec private applyFirst rules g =
    match rules with
    | (l,r)::rs -> 
        let res = StructureGraph.applyRule l r g
        if res = None then applyFirst rs g else res
    | [] -> None
let rec private applyRules rules g i =
    if i = Constants.ApplyLimit then g
    else
        let res = applyFirst rules g
        match res with
        | None -> g
        | Some(r) -> applyRules rules r (i+1)
let private replaceAt p f =
    List.mapi (fun i x -> if i = p then f x else x)
let private findPossible g ns : (((int*int) * (int*int) list) list) =
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
let private flip f y x = f x y
    
let private addEdge g =
    let ns = Graph.nodes g |> Set.toList
    if ns.Length = 0 then Graph.addEdge (0,0) (0,1) g
    else
        let allPossible = findPossible g ns
        let (a,possible) = allPossible.Item (rnd.Next allPossible.Length)
        let b = possible.Item (rnd.Next possible.Length)
        Graph.addEdge a b g
let private addToLeft rule =
    (addEdge <| fst rule, snd rule)
let private addToRight rule =
    (fst rule, addEdge <| snd rule)
let private remEdge g =
    let ns = Graph.nodes g |> Set.toList
    if ns.Length = 0 then Graph.addEdge (0,0) (0,1) g
    else
        let allPossible = ns |> List.map (fun n -> (n, StructureGraph.nodeNeighbourhood n g
                                                       |> Neighbourhood.toList |> List.choose id))
        let (a,possible) = allPossible.Item (rnd.Next allPossible.Length)
        let b = possible.Item (rnd.Next possible.Length)
        Graph.removeEdge a b g
let private remFrLeft rule =
    (addEdge <| fst rule, snd rule)
let private remFrRight rule =
    (fst rule, addEdge <| snd rule)


// Parameters for the algorithm
let initPop = List.init Constants.PopulationSize (fun _ -> [])
let muts  = [
    ((fun i -> i), 0);                                          // Nothing
    ((fun i ->                                                  // Add new rule if missing         
         if List.contains emptyRule i then i else emptyRule::i), 1);                               
    ((fun i -> replaceAt (rnd.Next i.Length) addToLeft i), 2);  // Add edge to a left
    ((fun i -> replaceAt (rnd.Next i.Length) addToRight i), 3); // Add edge to a right             
    ((fun i -> replaceAt (rnd.Next i.Length) remFrLeft i), 2);  // Remove edge from a left
    ((fun i -> replaceAt (rnd.Next i.Length) remFrRight i), 3); // Remove edge from a right
]
let eval (es : StructGraph -> (double * double) list) (rs : Genome) : double = 
    let g = applyRules rs seed 0
    List.map (fun (f,e) -> f * e) (es g) |> List.sum
    
    
let f i n l c = c i (int ((double n) * l))
let sel (n:int) ((i,s):int*double) : bool = (f i n 0.5 (<)) // || ((f i n 0.8 (>)) && (f i n 0.9 (<)))
let breed (a,b) = if rnd.Next(2) = 0 then a else b

let run evals =
    let res : (Genome*double) list = Evolution.train rnd muts (eval evals) sel breed initPop
    List.sortByDescending snd res
 |> List.take Constants.PrintTopResults
 |> List.map (fun (g,_) -> applyRules g seed 0)
 |> List.map (StructureGraph.toLat (StructureGraph.picker rnd))
