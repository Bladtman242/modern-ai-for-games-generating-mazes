module Block
open Neighbourhood

// vim: set sw=4 ts=4 et:

type Block
type ExitVect

// Temporary signature for creating a new block
val create : Block

// determines whether an exit vector has exits, be it a vector for a full
// block, or one or more sides
val hasExit : (ExitVect -> bool)

// Returns the vector of exits for the block
val exits : (Block -> ExitVect)

// Rotates the exit vector by 90 degrees counter-clockwise i times
val rotate : (int -> ExitVect -> ExitVect)


// Get the northerns exits
val north : (ExitVect -> ExitVect)

// Get the eastern exits
val east : (ExitVect -> ExitVect)

// Get the southern exits
val south : (ExitVect -> ExitVect)

// Get the western exits
val west : (ExitVect -> ExitVect)


// concatenate exit vectors, retaining the input order eg concat [[0;0]; [1;0]]
// = [0;0;1;0]
val concat : (seq<ExitVect> -> ExitVect)

// Takes to exitvectors; one representing the environment to match
// against and one representing the block that is to be matched, and a list of sides to match
// Returns the minimum number of rotations needed to make the second vector fit the first. None if no fit exists
// eg for blocksize = 2, fit [1;0;0;0;0;0;0;0] [true;false;false;false] [0;0;0;1;1;0;1;1] should return Some(2),
// because two rotations lines them up like this:
// env:          [1;0;0;0;0;0;0;0]
// bloc                  [0;0;0;1;
//                1;0;1;1]
// mask:         [1;1;0;0;0;0;0;0]
val fit : (Neighbourhood<ExitVect option> -> Block -> int list)
