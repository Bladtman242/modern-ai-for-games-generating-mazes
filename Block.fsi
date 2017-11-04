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

// Takes to exitvectors; one representing the  static environment to match
// against, and one representng the block that is to be matched
// Returns the number of 90 degree clockwise rotations needed to make the
// second vector fit the first. None if no fit exists
// eg for blocksize = 2, fit [1;0;0;0] [0;0;1;0;1;0] should return Some(2),
// because two rotations lines them up like this:
// [0;0;1;0;1;0]
//         [1;0;
//  0;0]
//  Note that the second vector must have length 4*BlockSize (unlike in the
//  example), and the first must have a length divisable in BlockSize
val fit : (ExitVect -> ExitVect -> int Option)
