
open Block

[<EntryPoint>]
let main argv = 
    System.Console.OutputEncoding <- System.Text.Encoding.Unicode

    let block = Block.createRandom (new System.Random())
    let rotated = Block.exits block |> Block.rotate 3
    printfn "Full %A" <| rotated
    printfn "North %A" <| Block.north rotated
    printfn "East %A" <| Block.east rotated
    printfn "South %A" <| Block.south rotated
    printfn "West %A" <| Block.west rotated
    
    //let matches = Block.fit (Block.exits block) [true;false;false;false] rotated
    //printfn "Fits itself with rotations %A" <| matches
    
    printfn "The graph looks like this: %A" <| Block.graph block
    
    let rec p list =
        match list with
        | [] -> ()
        | s :: ss -> 
            printfn "%A" s
            p ss
            
    p <| Block.print block
    
    0 
