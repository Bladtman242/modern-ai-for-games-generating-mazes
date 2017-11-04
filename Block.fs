module Block

open Constants

// vim: set sw=4 ts=4 et:

type ExitVect =
    private {
        vect : bool list
    }

type Block =
    private {
        exits : ExitVect
    }

let create = 
    { exits = { vect = [] } }

let exits (b : Block) = b.exits

let rotate (i : int) (v : ExitVect) = 
    v

let north (v : ExitVect) : ExitVect =
    let v' = List.take BlockSize v.vect
    { vect = v' }

let east (v : ExitVect) : ExitVect =
    let v' = List.take BlockSize (rotate 3 v).vect
    { vect = v' }

let south (v : ExitVect) : ExitVect =
    let v' = List.take BlockSize (rotate 2 v).vect
    { vect = v' }

let west (v : ExitVect) : ExitVect =
    let v' = List.take BlockSize (rotate 1 v).vect
    { vect = v' }

let append (v1 : ExitVect) (v2 : ExitVect) : ExitVect =
    let vect = List.append v1.vect v2.vect
    { vect = vect }

let concat (vs : seq<ExitVect>) =
    let newVect = Seq.map (fun ev -> ev.vect) vs
               |> Seq.concat
               |> Seq.toList
    { vect = newVect }


let rec fit (frame : ExitVect) (toFit : ExitVect) =
    // Helper function that determines if f contains tf, skipping
    // Constants.BlockSize elements of f between checks.
    let rec containsList (f : bool list) (tf : bool list) (rotations : int) =
        let fLength = List.length f
        let tfLength = List.length tf
        printfn "inputs %A and %A" f tf
        match f, tf with
        | f, tf when tfLength >= fLength ->
            printfn "comparing %A to %A" (List.take fLength tf) f
            if List.take fLength tf = f
            then Some(rotations)
            else containsList f (List.skip BlockSize tf) (rotations + 1)
        | _ ->
            printfn "Just none"
            None

    // Grab the actual vectors from the module wrapper,
    // pad the frame with itself, so that the contains function can consider
    // all rotations
    let f = frame.vect
    let tf = toFit.vect
    if List.length f > BlockSize * 4
       || List.length f % BlockSize <> 0
       || List.length tf <> BlockSize * 4
    then raise (System.ArgumentException "You're probably not using this function like you think you are")
    let tfPadded = List.append tf (List.take BlockSize tf)
    containsList f tfPadded 0

