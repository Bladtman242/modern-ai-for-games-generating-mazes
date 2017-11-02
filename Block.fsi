module Block

type Block
type ExitVect

val exits : (Block -> ExitVect)

// integer indicates number of 90-deg clockwise rotations
val rotate : (ExitVect -> int -> ExitVect)
val vect : (ExitVect -> int16)

// vim: set sw=4 ts=4 et:
