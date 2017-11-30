module Graph
open OptionExtensions

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
    let rec dist (visited : 'n Set) (frontier : (int*'n) List) (goal : 'n) : int option =
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
    let pairs = [ for a in (nodes g) do
                  for b in (nodes g) do
                      if a <> b then yield (a,b)]
    let dists = List.collect (fun (a,b) -> Option.toList <| distance a b g) pairs
    let sum = float <| List.sum dists
    let size = float <| List.length pairs
    sum / size

let pitfalls (g : 'n Graph) : float =
    0.0
