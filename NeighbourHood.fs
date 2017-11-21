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
            west = f n.west;
        }
let map2 (f : 'a -> int -> 'b) (n : Neighbourhood<'a>) : Neighbourhood<'b> = {
            north = f n.north 0;
            east = f n.east 1;
            south = f n.south 2;
            west = f n.west 3;
        }

let ofList (l : 'a list) : 'a Neighbourhood =
    if List.length l <> 4
    then raise (System.ArgumentException "Creating a neighbourhood with more than 4 sides may not do what you intend")
    else let [n;e;s;w] = l
         {north = n; east = e; south = s; west = w}

let rotate (times : int) (n : 'a Neighbourhood) : 'a Neighbourhood =
    let times' = times % 4
    let l = n.toList
    ofList <| List.append (List.skip (4-times') l) (List.take (4-times') l)
