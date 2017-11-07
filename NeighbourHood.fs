module Neighbourhood

type Neighbourhood<'a> = {
    north : 'a;
    east : 'a;
    south : 'a;
    west : 'a
} with
    member this.toList : 'a list = [this.north; this.east; this.south; this.west]
