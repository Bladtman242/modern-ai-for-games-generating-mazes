
open Block

[<EntryPoint>]
let main argv = 
    System.Console.OutputEncoding <- System.Text.Encoding.UTF8

    let block = Block.create (new System.Random()) [false;true;false;false ; false;false;true;false ; false;true;false;false ; false;false;false;false]
    
    let rec p list =
        match list with
        | [] -> ()
        | s :: ss -> 
            printfn "%s" s
            p ss
            
    p <| Block.print block 0
    p <| Block.print block 1
    p <| Block.print block 2
    p <| Block.print block 3
    
    
    0 
