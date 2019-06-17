module Home.MoveCamel
open Home.Types
open Home.Common

type ApplyRollResult = Map * int

let applyRoll (map : Map) (roll : DiceRoll) : ApplyRollResult =
    let {Count = rollNumber; Camel = camel} = roll
    // printfn "map: %A" map
    // printfn "roll: %A" roll
    
    let (camelMapIndex, Some (CamelStack camelStack)) = 
        map 
        |> Array.indexed
        |> Array.find (
            function 
            | (_, Some (CamelStack s)) -> s |> List.contains camel
            | _ -> false
        )

    let camelStackIndex = camelStack |> List.findIndex ((=) camel)

    // printfn "camel stack: %A" camelStack
    // printfn "index: %A" camelStackIndex

    let (camelsToMove, camelsToStay) = 
        camelStack 
        |> List.splitAt (camelStackIndex + 1)

    // printfn "move: %A" camelsToMove
    // printfn "stay: %A" camelsToStay

    let newCamelMapIndex = camelMapIndex + rollNumber;
    let fieldToLeave = 
        match camelsToStay with
        | [] -> None
        | camels -> (Some (CamelStack camels))

    let mapWithRemovedCamels = 
               map     
               |> setElement camelMapIndex 
                    fieldToLeave

    // printfn "map: %A" mapWithRemovedCamels

    let newField = 
        map 
        |> Array.tryItem newCamelMapIndex

    // printfn "new field: %A" newField

    let map =
        match newField with
        | None -> 
            map
        | Some field -> 
            match field with
            | Some (Tile t) -> failwith "TODO: Tile"
            | Some (CamelStack camels) -> 
                let newCamelStack =  camelsToMove @ camels
                mapWithRemovedCamels 
                |> setElement newCamelMapIndex (Some (CamelStack newCamelStack))
            | None -> 
                mapWithRemovedCamels 
                |> setElement newCamelMapIndex (Some (CamelStack camelsToMove))
    // printfn "map at end: %A" map
    // printfn "" 
    (map, camelMapIndex)        
