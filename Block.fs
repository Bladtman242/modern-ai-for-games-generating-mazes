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
        let a = if i < n/2 then (i/s)+i%s else i-i/2-1
        let b = if i < n/2 then a+1 else a+s
        yield (a,b)
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


let rec fit (neighbourVectors : Neighbourhood<ExitVect>) (sides : bool list) (toFit : ExitVect) : int list =
    let frame = concat [neighbourVectors.north; neighbourVectors.east; neighbourVectors.south; neighbourVectors.west]

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
    
    [for i in 0..3 -> tryMatch i toFit] |> List.choose id

