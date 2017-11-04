
open Block

[<EntryPoint>]
let main argv = 
    let block = Block.create
    let rotated = Block.exits block |> Block.rotate 3
    printfn "%A" <| rotated
    0 
