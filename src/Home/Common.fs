module Home.Common

let setElement key newEl = 
    Array.mapi (fun i v -> if i = key then newEl else v)

let insertElement key newEl list =
    let (before, after) = list |> List.splitAt key
    before @ [newEl] @ after
