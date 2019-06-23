module Home.State

open Elmish
open Types
open Domain.Types
open Common.Common

let allCamels = [Yellow; Blue; Orange; Green; White]


let initialMap : Map =
    Array.init (Constants.fieldsCount + Constants.maxRollDice)
        (function
        | 0 -> CamelStack allCamels |> Some
        | 3 -> PlusOne |> Plate |> Some
        | 6 -> MinusOne |> Plate |> Some
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

let createCalculationCmd map dicesLeft =
  let stageCommand = ServiceWorker.Api.CalculateStageWinChancesCommand {Map = map; DicesLeft = dicesLeft}
  let raceCommand = ServiceWorker.Api.CalculateRaceWinChancesCommand {Map = map; DicesLeft = dicesLeft}

  let stagePromiseCommand =
    Cmd.ofPromise
      ServiceWorkerClient.postMsg
      stageCommand
      (fun r -> StageWinChancesComputed r)
      (fun _ -> ComputationError)

  let racePromiseCommand =
    Cmd.ofPromise
      ServiceWorkerClient.postMsg
      raceCommand
      (fun r -> RaceWinChancesComputed r)
      (fun _ -> ComputationError)
  Cmd.batch [stagePromiseCommand; racePromiseCommand]

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
        }, createCalculationCmd newMap model.DicesLeft
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
      }, createCalculationCmd newMap model.DicesLeft


let update msg model : Model * Cmd<Msg> =
    match msg with
    | CamelDropped (place, camel) ->
        handleCamelDropped msg model (place, camel)

    | ResetDices ->
         {model with
            DicesLeft = allCamels
         }, createCalculationCmd model.Map allCamels
    | MarkDiceAsUsed(dice) ->
         let newDicesLeft = model.DicesLeft |> List.where (fun d -> d <> dice)
         {model with
            DicesLeft = newDicesLeft
         }, createCalculationCmd model.Map newDicesLeft

    | RaceWinChancesComputed(chances) -> {model with RaceWinChances = Some chances}, []
    | StageWinChancesComputed(chances) ->  {model with StageWinChances = Some chances}, []

    | ExistingPlateDropped(plateIndex, fieldIndex) ->
        let plate = model.Map.[plateIndex]
        let newMap =
          model.Map
          |> setElement fieldIndex plate
          |> setElement plateIndex None
        {model with Map = newMap}, createCalculationCmd newMap model.DicesLeft
    | FlipPlate (plateIndex) ->
      let plate = model.Map.[plateIndex]
      let newMap =
        match plate with
        | Some (Plate PlusOne) ->
          model.Map |> setElement plateIndex (Some (Plate MinusOne))
        | Some (Plate MinusOne) ->
          model.Map |> setElement plateIndex (Some (Plate PlusOne))
        | _ -> failwith "not a plate"
      {model with Map = newMap}, createCalculationCmd newMap model.DicesLeft
    | NewPlateDroppedOnBoard(plate, fieldIndex) ->
      let target = model.Map.[fieldIndex]
      match target with
      | (Some (CamelStack s)) -> model, []
      | _ ->
        let newMap =
          match plate with
          | PlusOne -> model.Map |> setElement fieldIndex (Some (Plate PlusOne))
          | MinusOne -> model.Map |> setElement fieldIndex (Some (Plate MinusOne))
        {model with Map = newMap}, createCalculationCmd newMap model.DicesLeft
    | RemovePlates ->
      let newMap =
        model.Map
        |> Array.map
          (fun f ->
            match f with
            | Some (Plate _) -> None
            | i -> i)
      {model with Map = newMap}, createCalculationCmd newMap model.DicesLeft

    | ComputationError -> failwith "Not Implemented"
    | RollDice(dice, count) ->
      let (newMap, newIndex) = Domain.MoveCamel.applyRoll model.Map {Count = count; Camel = dice}

      if newIndex < Domain.Types.Constants.fieldsCount then
        let newDicesLeft = model.DicesLeft |> List.where (fun d -> d <> dice)
        {model with
            Map = newMap
            DicesLeft = newDicesLeft
        }, createCalculationCmd newMap newDicesLeft

      else model, []
