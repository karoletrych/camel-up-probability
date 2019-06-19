module Home.State

open Elmish
open Types
open Common

let allCamels = [Yellow; Blue; Orange; Green; White]


let initialMap : Map =
    Array.init (Constants.fieldsCount + Constants.maxRollDice)
        (function
        | 0 -> CamelStack allCamels |> Some
        | _ -> None)

let initialState = {
  Map = initialMap
  DicesLeft = allCamels
  StageWinChances = None
  RaceWinChances = None
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


let handleCamelDropped msg model (droppedCamel, place) =
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
        StageWinChances = (Main.stageWinChances newMap model.DicesLeft) |> Some
        RaceWinChances = (Main.raceWinChances newMap model.DicesLeft) |> Some
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
          StageWinChances = (Main.stageWinChances newMap model.DicesLeft) |> Some
          RaceWinChances = (Main.raceWinChances newMap model.DicesLeft) |> Some
      }, []


let update msg model : Model * Cmd<Msg> =
    match msg with
    | CamelDropped (place, camel) ->
        handleCamelDropped msg model (place, camel)
    | ResetDices ->
         {model with
            DicesLeft = allCamels
            StageWinChances = (Main.stageWinChances model.Map allCamels) |> Some
            RaceWinChances = (Main.raceWinChances model.Map allCamels) |> Some
         }, []
    | MarkDiceAsUsed(usedDice) ->
         let newDicesLeft = model.DicesLeft |> List.where (fun d -> d <> usedDice)
         {model with
            DicesLeft = newDicesLeft
            StageWinChances = (Main.stageWinChances model.Map newDicesLeft) |> Some
            RaceWinChances = (Main.raceWinChances model.Map newDicesLeft) |> Some
         }, []
