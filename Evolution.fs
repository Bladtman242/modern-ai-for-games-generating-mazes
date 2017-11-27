module Evolution

type Population<'a> = 'a list
type Mutation<'a> = 'a -> 'a
type Evaluator<'a> = 'a -> double
type Selector = int -> int*double -> bool


let private fst' ((a,b,c) : 'a*'b*'c) : 'a = a 
let private snd' ((a,b,c) : 'a*'b*'c) : 'b = b 
let private trd' ((a,b,c) : 'a*'b*'c) : 'c = c 

// TODO specify chance of mutation getting picked
let mutate (rnd:System.Random) (muts:Mutation<'a> list) (pop:Population<'a>) : Population<'a> = 
    List.map (muts.Item (rnd.Next muts.Length)) pop

let evaluate (eval:Evaluator<'a>) (pop:Population<'a>) : ('a*double) list =
    List.map (fun e -> (e,eval e)) pop
    
let generation (rnd:System.Random) (muts:Mutation<'a> list) (eval:Evaluator<'a>) (sel:Selector) (pop:Population<'a>) : double*Population<'a> =
    let res = mutate rnd muts pop
              |> evaluate eval
              |> List.sortBy snd
              |> List.mapi (fun i (e,s) -> (e,sel pop.Length (pop.Length-i-1,s),s))
              |> List.filter snd'
              |> List.map (fun (e,_,s) -> (e,s))
    (List.average (List.map snd res),List.map fst res)
    
let train (rnd:System.Random) (gens:int) (muts:Mutation<'a> list) (eval:Evaluator<'a>) (sel:Selector) (pop:Population<'a>) : double*Population<'a> =
    let mutable result = (0.0,pop)
    for g = 0 to gens do
        result <- generation rnd muts eval sel <| snd result
        printfn "Fitness: %f" (fst result)
    result
