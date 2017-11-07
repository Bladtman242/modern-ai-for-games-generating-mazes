
open Block

[<EntryPoint>]
let main argv = 
    System.Console.OutputEncoding <- System.Text.Encoding.UTF8

    let block = Block.createRandom (new System.Random())
    
    let rec p list =
        match list with
        | [] -> ()
        | s :: ss -> 
            printfn "%A" s
            p ss
            
    p <| Block.print block
    
    0 
