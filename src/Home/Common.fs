module Common

let setElement key newEl = 
    Array.mapi (fun i v -> if i = key then newEl else v)
