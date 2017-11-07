module Block

open Constants
open Neighbourhood

// vim: set sw=4 ts=4 et:

type ExitVect =
    private {
        vect : bool list
    }

type Block =
    private {
        exits : ExitVect
        walls : bool list
    }
    
let numEdges = (Constants.BlockSize - 1) * Constants.BlockSize * 2

let createRandom (rnd : System.Random) : Block = 
    { 
        exits = { vect = [for _ in 0..(Constants.BlockSize*4)-1 -> (rnd.Next 2) = 0] };
        walls = [for _ in 0..numEdges-1 -> (rnd.Next 2) = 0];
    }

let graph (b : Block) : (int*int) list =
    let s = Constants.BlockSize
    let n = numEdges
    [for i in 0..n-1 do
        if b.walls.Item i then
            let a = if i < n/2 then (i/s)+(i%s)*s else i-n/2
            let b = if i < n/2 then a+1 else a+s
            yield (a,b)
    ]
    
let print (b : Block) : string list = 
    let s = Constants.BlockSize
    let n = s*2
    let edges = graph b
    let wall a b = List.exists (fun (i,j) -> (i = a && j = b) || (i = b && j = a)) edges
    
    let isWall x y = 
        match (x%2, y%2) with
        | (1,1) -> false
        | (0,1) -> 
            let i : int = ((x/2)-1) + (y/2)*s
            x = 0 || x = n || wall i (i+1)
        | (1,0) -> 
            let i : int = (x/2) + (y/2-1)*s
            y = 0 || y = n || wall i (i+s)
        | (_,_) -> true
    
    [for y in 0..n ->
        [for x in 0..n ->
            match (x%2, y%2) with
            | (1,1) -> " "
            | (0,1) -> if isWall x y then "\u2503" else " "
            | (1,0) -> if isWall x y then "\u2501" else " " 
            | (0,0) -> 
                match (y=0, x=0, y=n, x=n) with 
                | (true,false,false,false) -> if isWall x (y+1) then "\u2533" else "\u2501"
                | (true,false,false,true) -> "\u2513"
                | (false,false,false,true) -> if isWall (x-1) y then "\u252B" else "\u2503"
                | (false,false,true,true) -> "\u251B"
                | (false,false,true,false) -> if isWall x (y-1) then "\u253B" else "\u2501"
                | (false,true,true,false) -> "\u2517"
                | (false,true,false,false) -> if isWall (x+1) y then "\u2523" else "\u2503"
                | (true,true,false,false) -> "\u250F"
                | (_,_,_,_) -> 
                    match (isWall (x-1) y, isWall x (y-1), isWall (x+1) y, isWall x (y+1)) with
                    | (true, false, false, false)  -> "\u2578"
                    | (false, false, true, false)  -> "\u257A"
                    | (false, true, false, false)  -> "\u2579"
                    | (false, false, false, true)  -> "\u257B"
                    | (true, false, true, false)   -> "\u2501"
                    | (false, true, true, false)   -> "\u2517"
                    | (false, true, false, true)   -> "\u2503"
                    | (true, false, false, true)   -> "\u2513"
                    | (true, true, true, false)    -> "\u253B"
                    | (false, true, true, true)    -> "\u2523"
                    | (true, true, false, true)    -> "\u252B"
                    | (true, false, true, true)    -> "\u2533"
                    | (true, true, false, false)   -> "\u251B"
                    | (false, false, true, true)   -> "\u250F"
                    | (false, false, false, false) -> "\u00B7"
                    | (true, true, true, true)     -> "\u254B"
            | (_,_) -> ""
        ] |> List.fold (+) ""
    ]

let exits (b : Block) = b.exits

let hasExit (ev : ExitVect) : bool = List.exists id ev.vect

let rotate (i : int) (v : ExitVect) = 
    let a = List.take (Constants.BlockSize*i) v.vect
    let b = List.skip (Constants.BlockSize*i) v.vect    
    { vect = List.append b a }

let north (v : ExitVect) : ExitVect =
    let v' = List.take BlockSize v.vect
    { vect = v' }

let east (v : ExitVect) : ExitVect =
    let v' = List.take BlockSize (rotate 1 v).vect
    { vect = v' }

let south (v : ExitVect) : ExitVect =
    let v' = List.take BlockSize (rotate 2 v).vect
    { vect = v' }

let west (v : ExitVect) : ExitVect =
    let v' = List.take BlockSize (rotate 3 v).vect
    { vect = v' }

let append (v1 : ExitVect) (v2 : ExitVect) : ExitVect =
    let vect = List.append v1.vect v2.vect
    { vect = vect }

let concat (vs : seq<ExitVect>) =
    let newVect = Seq.map (fun ev -> ev.vect) vs
               |> Seq.concat
               |> Seq.toList
    { vect = newVect }


let rec fit (neighbourVectors : Neighbourhood<ExitVect option>) (toFit : Block) : int list =
    let neighbourList = [neighbourVectors.north; neighbourVectors.east; neighbourVectors.south; neighbourVectors.west]
    let sides = List.map Option.isSome neighbourList
    let frame = List.map Option.toList neighbourList
             |> List.concat
             |> concat

    // Helper function to check if there's a match
    // It's tail recursive. Sorry for the short names
    let rec matches a b m c =
        match a with
        | [] -> c
        | x::xs -> 
            match b with
            | [] -> raise (System.ArgumentException "ExitsVects are somehow not the same length")
            | y::ys -> 
                match m with
                | [] -> raise (System.ArgumentException "ExitsVects are somehow not the same length")
                | z::zs -> 
                    matches xs ys zs (c && (not z || y = x))

    // Create the mask
    let maskSide value = List.init Constants.BlockSize (fun i -> value)
    let mask = List.concat [for s in sides -> maskSide s]
    
    // Helper function that rotates the block if it doesn't match
    let rec tryMatch rot block =
        if matches frame.vect (rotate rot block).vect mask true
        then Some(rot)
        else None
    
    [for i in 0..3 -> tryMatch i (exits toFit)] |> List.choose id

