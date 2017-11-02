module Block

// vim: set sw=4 ts=4 et:

type Block
type ExitVect

// Temporary signature for creating a new block
val create : Block

// Returns the vector of exits for the block
val exits : (Block -> ExitVect)

// Rotates the exit vector by 90 degrees i times
val rotate : (int -> ExitVect -> ExitVect)

// Converts the exit vector to an integer for some reason I don't remember
val vect : (ExitVect -> int64)
