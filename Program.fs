
open Block

[<EntryPoint>]
let main argv = 
    let block = Block.create
    let rotated = Block.exits block |> Block.rotate 3
    printfn "Full %A" <| rotated
    printfn "North %A" <| Block.north rotated
    printfn "East %A" <| Block.east rotated
    printfn "South %A" <| Block.south rotated
    printfn "West %A" <| Block.west rotated
    
    let matches = Block.fit (Block.exits block) [true;false;false;false] rotated
    match matches with
    | Some(i) -> printfn "Fits itself with %A rotations" <| i
    | None -> printfn "Doesn't match itself"
    
    0 
