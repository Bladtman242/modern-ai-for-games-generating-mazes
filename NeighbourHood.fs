module Neighbourhood

type Neighbourhood<'a> = {
    north : 'a;
    east : 'a;
    south : 'a;
    west : 'a
} with
    member this.toList : 'a list = [this.north; this.east; this.south; this.west]
    
let create (n:'a) (e:'a) (s:'a) (w:'a) : Neighbourhood<'a> = {
    north = n;
    east = e;
    south = s;
    west = w;
}

let map (f : 'a -> 'b) (n : Neighbourhood<'a>) : Neighbourhood<'b> = {
            north = f n.north;
            east = f n.east;
            south = f n.south;
            west = f n.west
        }
