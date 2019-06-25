module Domain.MoveCamel
open Types
open Common.Common

let applyRoll (roll : DiceRoll) (map : Map) : (Map * int) =
    let {Count = rollNumber; Camel = camel} = roll
    // printfn "map: %A" map
    // printfn "roll: %A" roll

    let (camelMapIndex, camelStack) =
      map
      |> Array.indexed
      |> Array.choose (
          function
          | (i, Some (CamelStack s)) when  s |> List.contains camel -> Some (i,s)
          | _ -> None
      )
      |> Array.head

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

    match map.[newCamelMapIndex] with
    | Some (Plate p) ->
      match p with
      | PlusOne ->
        match map.[newCamelMapIndex + 1] with
        | Some (CamelStack camels) ->
          let newCamelStack =  camelsToMove @ camels
          mapWithRemovedCamels
          |> setElement (newCamelMapIndex + 1) (Some (CamelStack newCamelStack)), (newCamelMapIndex + 1)
        | None ->
          mapWithRemovedCamels
          |> setElement (newCamelMapIndex + 1) (Some (CamelStack camelsToMove)), (newCamelMapIndex + 1)
        | _ -> failwith "two plates next to each other"
      | MinusOne ->
        match map.[newCamelMapIndex - 1] with
        | Some (CamelStack camels) ->
          let newCamelStack =  camels @ camelsToMove
          mapWithRemovedCamels
          |> setElement (newCamelMapIndex - 1) (Some (CamelStack newCamelStack)), (newCamelMapIndex - 1)
        | None ->
          mapWithRemovedCamels
          |> setElement (newCamelMapIndex - 1) (Some (CamelStack camelsToMove)), (newCamelMapIndex - 1)
        | _ -> failwith "two plates next to each other"

    | Some (CamelStack camels) ->
        let newCamelStack =  camelsToMove @ camels
        mapWithRemovedCamels
        |> setElement newCamelMapIndex (Some (CamelStack newCamelStack)), newCamelMapIndex
    | None ->
        mapWithRemovedCamels
        |> setElement newCamelMapIndex (Some (CamelStack camelsToMove)), newCamelMapIndex
    // printfn "map at end: %A" map
    // printfn ""
