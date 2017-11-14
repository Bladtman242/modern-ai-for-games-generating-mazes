
open Block

[<EntryPoint>]
let main argv = 
    System.Console.OutputEncoding <- System.Text.Encoding.UTF8

    let block = Block.createRandom (new System.Random())
    
    let rec p list =
        match list with
        | [] -> ()
        | s :: ss -> 
            printfn "%s" s
            p ss
            
    p <| Block.print block
    
    printfn "%A" <| List.map Block.exitIndex [for i in 0..(Constants.BlockSize*4-1) -> i]
    
    0 
