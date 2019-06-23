module Home.View

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types
open Domain.Types

type SVG = SVGAttr

let fieldWidth = 20
let fieldHeight = 20

let plateMargin = 2 // TODO: remove this crap

let camelHeight = fieldHeight / 5
let camelWidth = fieldWidth

let coordsMapping = Map.ofList [
    (0, (4,2));
    (1, (4,3));
    (2, (4,4));
    (3, (3,4));
    (4, (2,4));
    (5, (1,4));
    (6, (0,4));
    (7, (0,3));
    (8, (0,2));
    (9, (0,1));
    (10, (0,0));
    (11, (1,0));
    (12, (2,0));
    (13, (3,0));
    (14, (4,0));
    (15, (4,1))]

let camelColor = function
| Camel.Blue -> "cyan"
| Camel.Orange -> "orange"
| Camel.Green -> "limegreen"
| Camel.White -> "white"
| Camel.Yellow -> "yellow"

let tryParseCamelFromId =
  function
  | "camel-blue" -> Camel.Blue |> Some
  | "camel-orange" -> Camel.Orange |> Some
  | "camel-green" -> Camel.Green |> Some
  | "camel-white" -> Camel.White |> Some
  | "camel-yellow" -> Camel.Yellow |> Some
  | _ -> None


open Fable.Import.React
open Fable.Import

module Draggable =
  let allowDrop(ev : DragEvent) =
    ev.preventDefault()

  let dragStart (ev : DragEvent) =
    let id = (ev.target :?> Fable.Import.Browser.Element).id
    ev.dataTransfer.setData("text", id) |> ignore

let camelStackView dispatch (camels : Camel list) fieldIndex =
    let (x, y) = coordsMapping |> Map.find fieldIndex

    let drop(ev : DragEvent) =
      ev.preventDefault()
      let targetCamelElement = (ev.target :?> Fable.Import.Browser.Element)
      let targetCamel = tryParseCamelFromId targetCamelElement.id
      let camelId = ev.dataTransfer.getData("text")
      let camel = tryParseCamelFromId camelId
      match targetCamel, camel with
      | Some targetCamel, Some camel -> CamelDropped (camel, OnTopOfCamel (targetCamel)) |> dispatch
      | _ -> ()

    camels
    |> List.rev
    |> List.mapi (
        fun camelIndex camel ->
        [div
              [
                Id (sprintf "camel-%s" (camel.ToString().ToLower() ))
                Style [
                    Width (sprintf "%d%%"(camelWidth))
                    Height (sprintf "%d%%"(camelHeight))
                    Left (sprintf "%d%%"(x * fieldWidth))
                    Top (sprintf "%d%%"((y+1) * fieldHeight - (camelIndex + 1) * camelHeight))
                    BackgroundColor (camelColor camel)
                    CSSProp.Position "absolute"
                    Border "1px solid black"
                    Cursor "pointer"
                    ]
                Draggable true
                OnDragStart Draggable.dragStart
                OnDragOver Draggable.allowDrop
                OnDrop drop
              ] []
        ])
    |> List.collect id

let plateView dispatch (plate : Plate) fieldIndex =
  let (x, y) = coordsMapping |> Map.find fieldIndex
  let text =
    match plate with
    | PlusOne -> "+1"
    | MinusOne -> "-1"
  div [
       Id (sprintf "plate-%d" fieldIndex)
       Class "plate"
       Draggable true
       OnDragStart Draggable.dragStart
       OnDragOver Draggable.allowDrop
       OnClick (fun _ -> FlipPlate fieldIndex |> dispatch)
       Style [
          Left (sprintf "%d%%" (x * fieldWidth + plateMargin))
          Top (sprintf "%d%%" (y * fieldHeight + plateMargin))
       ]
    ]
    [Fable.Helpers.React.HTMLNode.Text (text)]

open System.Text.RegularExpressions
let fieldView dispatch fieldIndex  =
    let drop(ev : DragEvent) =
      ev.preventDefault()
      let target = (ev.target :?> Fable.Import.Browser.Element)
      let fieldIndex = int (Regex.Match(target.id, @"\d+").Value);
      let draggedItemId = ev.dataTransfer.getData("text")

      let camelId = tryParseCamelFromId draggedItemId
      match camelId with
      | Some camelId -> CamelDropped (camelId, OnField (fieldIndex)) |> dispatch
      | None ->
        let isMinusPlate = Regex.IsMatch(draggedItemId, "minus-one-stack")
        let isPlusPlate = Regex.IsMatch(draggedItemId, "plus-one-stack")

        if isPlusPlate then
          NewPlateDroppedOnBoard (PlusOne, fieldIndex) |> dispatch
        else if isMinusPlate then
          NewPlateDroppedOnBoard (MinusOne, fieldIndex) |> dispatch
        else
          let plateIndex = int (Regex.Match(draggedItemId, @"\d+").Value)
          ExistingPlateDropped (plateIndex, fieldIndex) |> dispatch

    let field ((x,y),i) =
        [div
          [
              Id (sprintf "field-%d" i)
              Style [
                  Width (sprintf "%d%%"(fieldWidth))
                  Height (sprintf "%d%%"(fieldHeight))
                  Left (sprintf "%d%%"(x*fieldWidth))
                  Top (sprintf "%d%%"(y*fieldHeight))
                  BackgroundColor "#F0E68C"
                  CSSProp.StrokeWidth "1"
                  CSSProp.Stroke "#000000"
                  CSSProp.Position "absolute"
                  Display "flex"
                  AlignItems "flex-end"
                  JustifyContent "center"
                  Border "2px solid black"
              ]
              OnDrop drop
              OnDragOver Draggable.allowDrop
          ]
          [Fable.Helpers.React.HTMLNode.Text (string (i+1))]
        ]

    let coord = coordsMapping |> Map.find fieldIndex
    field (coord, fieldIndex)


let dicesView dispatch dice =
  div [] (
    [
      "X", MarkDiceAsUsed dice;
      "1", RollDice (dice, 1);
      "2", RollDice (dice, 2);
      "3", RollDice (dice, 3)
    ]
    |> List.map
        (fun (label, command) ->
        button [
            OnClick (fun _ -> command |> dispatch)
            Style [
              BackgroundColor (camelColor dice)
            ]
          ] [
            Fable.Helpers.React.HTMLNode.Text label
          ])
    )


let resetButtonView dispatch =
    button [
        OnClick (fun _ -> ResetDices |> dispatch)
        Class "reset-button"
    ] [
      Fable.Helpers.React.HTMLNode.Text "RESET"
    ]

let boardCenterView (dicesLeft : Camel list) dispatch =
    div
        [
            Id "pyramid"
            Style [
                Top (sprintf "%d%%"(fieldHeight))
                Left (sprintf "%d%%"(fieldWidth))
                Width (sprintf "%d%%"(100 - 2 * fieldWidth))
                Height (sprintf "%d%%"(100 - 2 * fieldHeight))
                Background "#FFFACD"
                Position "absolute"
                Display "flex"
                FlexDirection "column"
            ]
        ]
        [
            resetButtonView dispatch
            div[ Class "dices-grid"]
              (dicesLeft |> List.map (dicesView dispatch))
        ]

let boardView model dispatch =
  div [ Style [Width "500px"; Height "500px"; CSSProp.Position "relative"]  ]
    (
      let fieldContents =
        model.Map
        |> List.ofArray
        |> List.take Domain.Types.Constants.fieldsCount
        |> List.indexed
        |> List.map (function
          | (i, Some (CamelStack stack)) -> camelStackView dispatch stack i
          | (i, Some (Plate p)) -> [plateView dispatch p i]
          | (_, None) -> [])
        |> List.collect id

     ([0..15] |> List.collect (fieldView dispatch))
     @ fieldContents
     @ [boardCenterView model.DicesLeft dispatch]
    )

let chancesView title (model : (Camel * float) list option) =
  match model with
  | Some chances ->
    div [
        Style[MarginLeft "2em"]
    ]
        [
          div
              [Style [FontSize "1.5em"]]
              [Fable.Helpers.React.HTMLNode.Text title]
          ul
            []
            (
            chances
              |> List.map (fun (c, f) ->
                let text = sprintf "%.5g" (100. * f)
                li [Style [Background (camelColor c); FontSize "1.5em"]; ] [ Fable.Helpers.React.HTMLNode.Text text])
            )
        ]
  | None -> null

let chancesSummaryView (model : Model) =
  div [Class "chances-summary"] [
    chancesView "STAGE(%)" model.StageWinChances
    chancesView "RACE(%)" model.RaceWinChances
  ]

let platePanel dispatch =
  div [Class "plate-panel"] [
    div [
       Id "plus-one-stack"
       Class "plate-stack"
       Draggable true
       OnDragStart Draggable.dragStart]
      [Fable.Helpers.React.HTMLNode.Text ("+1")]
    div [
      Id "minus-one-stack"
      Class "plate-stack"
      Draggable true
      OnDragStart Draggable.dragStart]
      [Fable.Helpers.React.HTMLNode.Text ("-1")]
    button [
      OnClick (fun _ -> dispatch RemovePlates)
    ] [Fable.Helpers.React.HTMLNode.Text ("Remove plates")]
  ]

let sidePanel model dispatch =
  div [Class "side-panel"] [
      chancesSummaryView model
      platePanel dispatch
  ]

let root (model : Model) (dispatch : Msg -> unit) =
  div
    [Style [Display "flex"] ]
    [
      boardView model dispatch
      sidePanel model dispatch
    ]
