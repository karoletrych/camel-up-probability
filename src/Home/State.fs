module Home.State

open Elmish
open Home.Types
open Common
open Main

let allCamels = [Yellow; Blue; Orange; Green; White]

let initialMap : Map =
    Array.init 16
        (function
        | 0 -> CamelStack allCamels |> Some
        | _ -> None)

let initialState = {
  Map = initialMap
  DicesLeft = allCamels
  StageWinChances = None
}

let init () : Model * Cmd<Msg> =
  initialState, []

let findCamelStack camel model =
  model
  |> Array.indexed
  |> Array.choose (fun f ->
                    match f with
                    | (i, Some (CamelStack stack)) ->
                      if stack |> List.contains camel then Some (i, stack) else None
                    | _ -> None)
  |> Array.head


let update msg model : Model * Cmd<Msg> =
    let calculateChances map =
      Main.stageWinChances map (model.DicesLeft)
    match msg with
    | CamelDropped (droppedCamel, place) ->
      match place with
      | OnTopOfCamel targetCamel ->
        let newMap =
          let map = model.Map
          let (oldStackIndex, oldStack) = map |> findCamelStack droppedCamel
          let (targetStackIndex, targetStack) = map |> findCamelStack targetCamel

          let targetCamelPosition = targetStack |> List.findIndex (fun c -> c = targetCamel)
          let newMap =
              let updatedOldStack =
                  oldStack
                  |> List.where (fun c -> c <> droppedCamel)
                  |> CamelStack |> Some
              let updatedTargetStack =
                targetStack
                |> List.where (fun c -> c <> droppedCamel)
                |> insertElement targetCamelPosition droppedCamel
                |> CamelStack |> Some
              map
              |> setElement oldStackIndex updatedOldStack
              |> setElement targetStackIndex updatedTargetStack
          newMap
        {model with
          Map = newMap
          StageWinChances = calculateChances newMap |> Some
          }, []
      | OnField fieldIndex ->
        let newMap =
          let map = model.Map
          let (oldStackIndex, oldStack) = map |> findCamelStack droppedCamel
          let oldStackUpdated =
            oldStack
            |> List.where (fun c -> c <> droppedCamel)
            |> CamelStack |> Some
          let newField = map.[fieldIndex]
          let newFieldUpdated =
            match newField with
            | Some (CamelStack s) -> CamelStack (droppedCamel :: (s |> List.where (fun c -> c <> droppedCamel)))
            | None -> CamelStack [droppedCamel]
            |> Some

          let newMap =
            map
            |> setElement oldStackIndex oldStackUpdated
            |> setElement fieldIndex newFieldUpdated
          newMap
        {model with
            Map = newMap
            StageWinChances = calculateChances newMap |> Some
        }, []
