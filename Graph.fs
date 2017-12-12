module Graph
open OptionExtensions
open System.Collections.Generic

// The Graph moudle implements a simple undirected graph.
// The graph is "persistent", in that operations to alter a graph returns a new
// graph with the desired changes, and leaves the original graph intact
//
// The implementation is based on "doubly-linked" adjacency lists (represented
// as sets in a map). If a and b are adjacent, the "adjencencies" map contains
// k/v entry a/[a set that contains b], _and vice versa_. The duplication
// allows for fast(er) adjacency lookups. If needed, we can easily modify this
// implementation to a directed graph, and then build an undirected graph on
// top of it, but at time of writing, it seems we only need undirected graphs.

type Graph<'n when 'n : comparison> =
    private {
        adjacencies : Map<'n,Set<'n>>
    }

// The empty graph. Used to build graphs with the combinators in this module
let empty : Graph<'n> = { adjacencies = Map.empty }

let private flip f x y = f y x

// Returns the set of nodes that are adjacent to n.
// Note that if n is not in the graph, the empty set will be returned
let adjacentTo (n : 'n) (g : Graph<'n>) : Set<'n> =
    Map.tryFind n g.adjacencies
 |> Option.getOrElse Set.empty

// Returns true if a and b are adjacent.
// Note that if either a or b is not in the graph, areAdjacent returns false.
let areAdjacent (a : 'n) (b : 'n) (g : Graph<'n>) =
   Set.contains b <| adjacentTo a g

// Returns a new graph where a and b are adjacent.
// a and b are effectively added to the graph, if they were not already present
let addEdge (a : 'n) (b : 'n) (g : Graph<'n>) : Graph<'n> =
    let aAdj = adjacentTo a g
    let bAdj = adjacentTo b g
    let adj' = Map.add a (Set.add b aAdj) g.adjacencies
            |> Map.add b (Set.add a bAdj)
    { adjacencies = adj' }

let removeEdge (a : 'n) (b : 'n) (g : Graph<'n>) : Graph<'n> =
    let aAdj = Set.remove b (adjacentTo a g)
    let bAdj = Set.remove a (adjacentTo b g)
    let adjWithoutAB = if aAdj = Set.empty
                       then Map.remove a g.adjacencies
                       else Map.add a aAdj g.adjacencies
    let adj' = if bAdj = Set.empty
               then Map.remove b adjWithoutAB
               else Map.add b bAdj adjWithoutAB

    { adjacencies = adj' }

let nodes (g: 'n Graph) : 'n Set =
    Map.toList g.adjacencies |> List.map fst |> Set.ofList

let contains (n : 'n) (g : 'n Graph) : bool = Map.containsKey n g.adjacencies

let avgDegree (g : 'n Graph) : float = Map.toList g.adjacencies
                                    |> List.map (snd >> Set.count)
                                    |>  List.sum
                                    |> fun s -> (float s) / (float (Map.count g.adjacencies))

let distance (a : 'n) (b : 'n) (g : 'n Graph) : int option =
    let rec dist (visited : 'n Set) (frontier : (int*'n) list) (goal : 'n) : int option =
        if List.isEmpty frontier then None
        else if Seq.contains (snd <| List.head frontier) visited
             then dist visited (List.tail frontier) goal
             else let (d,n) = List.head frontier
                  if goal = n then Some (d)
                  else let neighbours = Seq.toList (adjacentTo n g)
                                     |> List.map (fun n -> (d+1,n))
                       dist (Set.add n visited) (List.tail frontier @ neighbours) goal
    dist Set.empty [(0,a)] b

let avgDistance (g : 'n Graph) : float =
    let distancesSum (n : 'n) (g : 'n Graph) : int =
        let q = new Queue<'n*int> ();
        adjacentTo n g |> Set.map (fun n -> (n, 1)) |> Set.iter (q.Enqueue)
        let rec bfs (res : Map<'n,int>) : Map<'n,int> =
            if 0 = q.Count then res
            else let (next,len) = q.Dequeue ()
                 if Map.containsKey next res || n = next then bfs res
                 else adjacentTo next g |> Seq.map (fun n -> (n,len+1)) |> Seq.iter (q.Enqueue); bfs (Map.add next (len) res)

        bfs Map.empty |> Map.fold (fun s _ d -> d+s) 0
    let nodes = nodes g
    let size = Set.count nodes |> double
    let sum = Set.toList nodes
           |> List.sumBy (fun n -> distancesSum n g)
           |> double

    sum / (2.0 * size * (size - 1.0))

// travel will return the walk (node list) from node b to the first node that
// has more than 2 neighbours. Exclude is used to exclude a neighbour to a, to
// prevent travel from going backwards (recall that if a is a neighbour to b,
// then b is a neighbour to a as well), or to determine the travel direction,
// you wish to travel from a node with more than one neighbour
let travelExclude (a : 'n) (exclude : 'n Set) (g : 'n Graph) : 'n list =
    //inner helper function tracks the result-path along as an accumulation
    //param, so that it is reversed without extra computations
    //Point of interest; this also makes the function tail recursive
    let rec innerTravel (a : 'n) (exclude : 'n Set) (acc: 'n list) (g : 'n Graph) : 'n list =
        let adjs = adjacentTo a g
                |> (flip Set.difference) exclude
        match Set.count adjs with
        | 1 -> innerTravel (Set.maxElement adjs) (Set.singleton a) (a :: acc) g
        | _ -> a :: acc
    innerTravel a exclude [] g

// convenience wrapper for travelExclude, for the special case where a has
// exactly one neighbour
let travel (a : 'n) (g : 'n Graph) : 'n list =
    travelExclude a Set.empty g

let culDeSacs (g : 'n Graph) : 'n list list =
    Map.filter (fun k adjs -> 1 = Set.count adjs) g.adjacencies
 |> Map.map (fun k adjs -> travel k g) //note that adjs is always the singleton set here!
 |> Map.toList
 |> List.map snd

let culDeSacsCountLengts (g : 'n Graph) : int * int list =
    let sacs = culDeSacs g
    let numSacs = List.length sacs
    if numSacs = 0 then (0,[])
    else let lengths = List.map (List.length) sacs
                         |> List.sortDescending
         (numSacs,lengths)

let trees (g : 'n Graph) =
    //let addPath n path map : Map<'n,(int * 'n list) list> =
    //    let modified = Map.tryFind n map
    //                |> Option.map (fun (s,l) -> (s + List.length path,path :: l))
    //    Map.add n (Option.getOrElse [(List.length path, path)] modified) map

    //let keepTree n t map : Map<'n,(int * 'n list) list> =
    //    Map.tryFind n map
    // |> Option.map (fun (s,paths) -> (s + List.append t))
    // |> Option.getOrElse t
    // |> fun newPaths -> Map.add n newPaths map


    let rec advanceRoots (ts : Map<'n ,(int * ('n list list))>) =
        let (reachedSet, advances) : ('n Set * ('n * 'n list) list) =
            Map.fold (fun (reached,advances) root (_, paths) ->
                        // if some other tree  already advanced to this root, this
                        // shouldn't also advance
                        if Set.contains root reached then (reached, advances)
                        else let examinedChildren : 'n Set = Set.ofList <| List.map (List.item 1) paths
                             let nonExaminedCount : int = (Set.count (adjacentTo root g)) - (Set.count examinedChildren)
                             if 1 <> nonExaminedCount then (reached, advances)
                             else let newPath = travelExclude root examinedChildren g
                                  let newRoot = List.head newPath
                                  (Set.add newRoot reached, (root,newPath) :: advances)) (Set.empty,[]) ts
        let advWithSizes = List.fold (fun m (oldRoot,newPath) ->
                                        let newRoot = List.head newPath
                                        let newPathSize = List.length newPath
                                        let (newTreeSize, newTreePaths) = Map.tryFind newRoot m
                                                                       |> Option.map (fun (s,paths) -> (s + newPathSize - 2, newPath :: paths))
                                                                       |> Option.getOrElse (newPathSize - 1, [newPath])
                                        let oldTreeSize = fst <| Map.find oldRoot ts
                                        if Map.containsKey newRoot ts && not (Map.containsKey newRoot m)
                                        then let (existingSize,existingPaths) = Map.find newRoot ts
                                             Map.add (newRoot) (existingSize + newTreeSize + oldTreeSize - 1, List.append newTreePaths existingPaths) m
                                        else let newSize = oldTreeSize + newTreeSize
                                             Map.add newRoot (newSize, newTreePaths) m) Map.empty advances
        let consolidated : Map<'n,int * 'n list list>= List.fold (fun m (oldRoot, newPath) ->
                                                                   let newRoot = List.head newPath
                                                                   Map.remove oldRoot m
                                                                |> Map.add newRoot (Map.find newRoot advWithSizes)) ts advances
        if 0 < Set.count reachedSet then advanceRoots advWithSizes
        else ts
    let sacsAsTrees = List.fold (fun m path -> let root = List.head path
                                               Map.tryFind root m
                                            |> Option.map (fun (s,paths) -> (s + List.length path - 1, path :: paths))
                                            |> Option.getOrElse (List.length path, [path])
                                            |> fun v -> Map.add root v m) Map.empty (culDeSacs g)
    advanceRoots sacsAsTrees
 |> Map.fold (fun l _ (size,_) -> size :: l) []

let pitfalls (g : 'n Graph) : int =
    let isPitfall n = 
        let progress ((ns,hist) : ('n Set * 'n Set)) : ('n Set * 'n Set) =
            (Set.fold (fun acc n -> 
                           g.adjacencies.Item n 
                           |> Set.filter (fun n -> not <| hist.Contains n) 
                           |> Set.union acc
                       ) Set.empty ns, Set.union hist ns)
        progress (Set.ofList [n],Set.empty) 
            |> progress 
            |> fst 
            |> Set.exists (fun m -> Set.isSubset (g.adjacencies.Item n) (g.adjacencies.Item m))
    g.adjacencies |> Map.toList |> List.map fst |> List.where isPitfall |> List.length

let treesCountLength (g : 'n Graph) : (int * int) =
    let treeSizes = trees g
    let numTrees = List.length treeSizes
    if numTrees = 0 then (0,0)
    else let medianLength = List.sort treeSizes
                         |> List.item (numTrees/2)
         (numTrees,medianLength)

let boundingBoxSize (g : (int*int) Graph) : (int*int) =
    let coords : (int*int) list = Map.toList g.adjacencies |> List.map fst
    if List.isEmpty coords then (0,0)
    else let minX = List.minBy fst coords |> fst
         let maxX = List.maxBy fst coords |> fst
         let minY = List.minBy snd coords |> snd
         let maxY = List.maxBy snd coords |> snd
         (maxX-minX, maxY-minY)
