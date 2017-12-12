module Evolution

type Population<'a> = 'a list
type Mutation<'a> = 'a -> 'a
type Evaluator<'a> = 'a -> double
type Selector = int -> int*double -> bool
type Breeder<'a> = 'a*'a -> 'a


let private fst' ((a,b,c) : 'a*'b*'c) : 'a = a 
let private snd' ((a,b,c) : 'a*'b*'c) : 'b = b 
let private trd' ((a,b,c) : 'a*'b*'c) : 'c = c 

let private pick (rnd:System.Random) (xs:'a list) : 'a = xs.Item (rnd.Next (xs.Length))
let private pickWeighted (rnd:System.Random) (xs:('a*int) list) : 'a = 
    let mutable i = rnd.Next (List.sumBy snd xs)
    List.skipWhile (fun (x,w) -> i <- i-w; i >= 0) xs
    |> List.item 0 |> fst

let mutate (rnd:System.Random) (muts:(Mutation<'a>*int) list) (pop:Population<'a>) : Population<'a> = 
    List.map (fun e -> pickWeighted rnd muts <| e) pop

let evaluate (eval:Evaluator<'a>) (pop:Population<'a>) : ('a*double) list =
    List.map (fun e -> (e,eval e)) pop
    
let generation (rnd:System.Random) (muts:(Mutation<'a>*int) list) (eval:Evaluator<'a>) (sel:Selector) (breed:Breeder<'a>) (pop:('a*double) list) : ('a*double) list =
    let res = pop
              |> List.mapi (fun i (e,s) -> (e,sel pop.Length (pop.Length-i-1,s),s))
              |> List.filter snd'
              |> List.map (fun (e,_,s) -> (e,s))
    let children = List.init (pop.Length - res.Length) (fun _ -> breed (fst (pick rnd res), fst (pick rnd res)))
                   |> mutate rnd muts
    let newPop = res @ evaluate eval children
    List.sortBy snd newPop

let train (rnd:System.Random) (muts:(Mutation<'a>*int) list) (eval:Evaluator<'a>) (sel:Selector) (breed:Breeder<'a>) (pop:Population<'a>) : ('a*double) list =
    let mutable result = evaluate eval pop
    let mutable lastAvg= List.average (List.map snd result)
    let mutable stale = 0
    for g = 0 to Constants.MaxGenerations do
        if g < Constants.MinGenerations || stale <> Constants.StopWhenStaleFor then
            let res = generation rnd muts eval sel breed result
            let best = snd <| List.last res
            let median = snd <| List.item ((List.length result) / 2) result
            let avg = List.average <| List.map snd res
            if avg = lastAvg then stale <- stale + 1 else stale <- 0
            result <- res
            lastAvg <- avg
            printfn "Fitness in generation %d: average %f, median %f, best %f" g avg median best
    printfn "Done"
    result
