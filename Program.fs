
open Block

[<EntryPoint>]
let main argv = 
    let block = Block.create
    let rotated = Block.exits block |> Block.rotate 2
    printfn "%A" <| Block.vect rotated
    0 
