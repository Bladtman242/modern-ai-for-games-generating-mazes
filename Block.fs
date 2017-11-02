module Block

open Constants

// vim: set sw=4 ts=4 et:

type ExitVect =
    private {
        vect : int64
    }

type Block =
    private {
        exits : ExitVect
    }
    
let create = 
    { exits = { vect = 0L } }

let exits (b : Block) = b.exits

let rotate (i : int) (v : ExitVect) = 
    v

let vect(v : ExitVect) = 
    v.vect
