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


let update msg model : Model * Cmd<Msg> =
    match msg with
    | CamelDropped (droppedCamel, place) ->
      match place with
      | OnTopOfCamel c ->
        model, []
      | OnField fieldIndex ->
        let (oldStackIndex, oldStack) = 
          model 
          |> Array.indexed
          |> Array.choose (fun f -> 
                              match f with 
                              | (i, Some (CamelStack stack)) -> 
                                if stack |> List.contains droppedCamel then Some (i, stack) else None
                              | _ -> None)
          |> Array.head                            
        let oldStackUpdated = (oldStack) |> List.where (fun c -> c <> (droppedCamel)) |> CamelStack |> Some
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
