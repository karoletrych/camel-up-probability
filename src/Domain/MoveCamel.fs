module Domain.MoveCamel
open Types
open Common.Common

let applyRoll (roll : DiceRoll) (map : Map) : (Map * int) =
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

    // printfn "new field: %A" newField

    let map =
      match map.[newCamelMapIndex] with
      | Some (Plate p) ->
        match p with
        | PlusOne ->
          match map.[newCamelMapIndex + 1] with
          | Some (CamelStack camels) ->
            let newCamelStack =  camelsToMove @ camels
            mapWithRemovedCamels
            |> setElement (newCamelMapIndex + 1) (Some (CamelStack newCamelStack))
          | None ->
            mapWithRemovedCamels
            |> setElement (newCamelMapIndex + 1) (Some (CamelStack camelsToMove))
          | _ -> failwith "two plates next to each other"
        | MinusOne ->
          match map.[newCamelMapIndex - 1] with
          | Some (CamelStack camels) ->
            let newCamelStack =  camels @ camelsToMove
            mapWithRemovedCamels
            |> setElement (newCamelMapIndex - 1) (Some (CamelStack newCamelStack))
          | None ->
            mapWithRemovedCamels
            |> setElement (newCamelMapIndex - 1) (Some (CamelStack camelsToMove))
          | _ -> failwith "two plates next to each other"

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
