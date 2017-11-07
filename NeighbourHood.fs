module Neighbourhood

type Neighbourhood<'a> = {
    north : 'a;
    east : 'a;
    south : 'a;
    west : 'a
} with
    member this.toList : 'a list = [this.north; this.east; this.south; this.west]

let map (f : 'a -> 'b) (n : Neighbourhood<'a>) : Neighbourhood<'b> = {
            north = f n.north;
            east = f n.east;
            south = f n.south;
            west = f n.west
        }
// vim: set sw=4 ts=4 et:
