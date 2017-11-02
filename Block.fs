module Block

type ExitVect =
    private {
        vect : int16
    }

type Block =
    private {
        exitVect : ExitVect
    }

let exits (b : Block) = b.exitVect

// vim: set sw=4 ts=4 et:
