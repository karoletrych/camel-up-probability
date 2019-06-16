module Home.State

open Elmish
open Types
open Common

let allCamels = [Yellow; Blue; Orange; Green; White]

let initialState : Model = 
    Array.init 16
        (function
        | 0 -> CamelStack allCamels |> Some
        | _ -> None)

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
    match msg with
    | CamelDropped (droppedCamel, place) ->
      match place with
      | OnTopOfCamel targetCamel ->
        let (oldStackIndex, oldStack) = model |> findCamelStack droppedCamel
        let (targetStackIndex, targetStack) = model |> findCamelStack targetCamel

        let targetCamelPosition = targetStack |> List.findIndex (fun c -> c = targetCamel)
        let newModel =
            let updatedOldStack =
                oldStack
                |> List.where (fun c -> c <> droppedCamel)
                |> CamelStack |> Some
            let updatedTargetStack = 
              targetStack
              |> List.where (fun c -> c <> droppedCamel)
              |> insertElement targetCamelPosition droppedCamel
              |> CamelStack |> Some
            model
            |> setElement oldStackIndex updatedOldStack
            |> setElement targetStackIndex updatedTargetStack

        newModel, []
      | OnField fieldIndex ->
        let (oldStackIndex, oldStack) = model |> findCamelStack droppedCamel
        let oldStackUpdated = 
          oldStack 
          |> List.where (fun c -> c <> droppedCamel) 
          |> CamelStack |> Some
        let newField = model.[fieldIndex]
        let newFieldUpdated = 
          match newField with
          | Some (CamelStack s) -> CamelStack (droppedCamel :: s)
          | None -> CamelStack [droppedCamel]
          |> Some

        let newModel = 
          model 
          |> setElement fieldIndex newFieldUpdated
          |> setElement oldStackIndex oldStackUpdated
        
        newModel, []
