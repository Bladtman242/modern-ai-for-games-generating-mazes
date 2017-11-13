module OptionExtensions
module Option =
    let getOrElse (els : 'a) (opt : Option<'a>) : 'a =
        Option.orElseWith (fun _ -> Some els) opt |> Option.get
