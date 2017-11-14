
open Block

[<EntryPoint>]
let main argv = 
    System.Console.OutputEncoding <- System.Text.Encoding.UTF8

    let rnd = System.Random 1
    let b1 = Block.createRandom rnd
    let b2 = Block.createRandom rnd
    let b3 = Block.createRandom rnd
    let b4 = Block.createRandom rnd

    let l1 = Lattice.placeBlock b1 (0,0) Lattice.emptyLat
    let l2 = Option.bind (Lattice.placeBlock b2 (1,0)) l1
    let l3 = Option.bind (Lattice.placeBlock b3 (0,1)) l2
    let l4 = Option.bind (Lattice.placeBlock b4 (1,1)) l3

    
    ignore <| Option.map Lattice.print l1
    ignore <| Option.map Lattice.print l2

    Block.print b2 0 |> List.map (printfn "%s")
    0
