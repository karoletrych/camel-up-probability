module Home.View

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types
open Domain.Types

type SVG = SVGAttr

let fieldWidth = 20
let fieldHeight = 20


let camelHeight = fieldHeight / 5
let camelWidth = fieldWidth
let fieldsCount = 16


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

let camelFromId id =
  match id with
  | "camel-blue" -> Camel.Blue
  | "camel-orange" -> Camel.Orange
  | "camel-green" -> Camel.Green
  | "camel-white" -> Camel.White
  | "camel-yellow" -> Camel.Yellow
  | _ -> failwith (sprintf "invalid camel id: %s" id)

open Fable.Import.React
open Fable.Import

let allowDrop(ev : DragEvent) =
  ev.preventDefault()


let camelStack dispatch (camels : Camel list) fieldIndex =
    let (x, y) = coordsMapping |> Map.find fieldIndex

    let dragStart (ev : DragEvent) =
      let id = (ev.target :?> Fable.Import.Browser.Element).id
      ev.dataTransfer.setData("text", id) |> ignore

    let drop(ev : DragEvent) =
      ev.preventDefault()
      let targetCamelElement = (ev.target :?> Fable.Import.Browser.Element)
      let targetCamel = camelFromId targetCamelElement.id
      let camelId = ev.dataTransfer.getData("text")
      let camel = camelFromId camelId
      CamelDropped (camel, OnTopOfCamel (targetCamel)) |> dispatch

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
                OnDragStart dragStart
                OnDragOver allowDrop
                OnDrop drop
              ] []
        ])

let field dispatch fieldIndex  =
    let drop(ev : DragEvent) =
      ev.preventDefault()
      let target = (ev.target :?> Fable.Import.Browser.Element)
      let fieldIndex = int (System.Text.RegularExpressions.Regex.Match(target.id, @"\d+").Value);
      let camelId = ev.dataTransfer.getData("text")
      let camel = camelFromId camelId
      CamelDropped (camel, OnField (fieldIndex)) |> dispatch

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
              OnDragOver allowDrop
          ]
          [Fable.Helpers.React.HTMLNode.Text (string (i+1))]
        ]

    let coord = coordsMapping |> Map.find fieldIndex
    field (coord, fieldIndex)


let camelDices dispatch dice =
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


let resetButton dispatch =
    button [
        OnClick (fun _ -> ResetDices |> dispatch)
        Class "reset-button"
        Style [

        ]
    ] [
      Fable.Helpers.React.HTMLNode.Text "RESET"
    ]

let boardCenter (dicesLeft : Camel list) dispatch =
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
            resetButton dispatch
            div[ Class "dices-grid"]
              (dicesLeft |> List.map (camelDices dispatch))
        ]

let board model dispatch =
  div [
      Style [
          Width "500px"
          Height "500px"
          CSSProp.Position "relative"
          ]
  ]
    (
     let camelsIndexed =
         model.Map
         |> Array.toList
         |> List.take fieldsCount
         |> List.indexed
         |> List.choose (
                  function
                  | (i, Some (CamelStack stack)) -> Some (i, stack)
                  | _ -> None
                  )
     let renderedCamels =
        camelsIndexed
        |> List.collect (fun (i, camels) -> camelStack dispatch camels i)
        |> List.collect id
     ([0..15] |> List.collect (field dispatch))
     @ renderedCamels
     @ [boardCenter model.DicesLeft dispatch]
   )

let chancesSummary title (model : (Camel * float) list option) =
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
                let text = sprintf "%f" f
                li [Style [Background (camelColor c); FontSize "1.5em"]; ] [ Fable.Helpers.React.HTMLNode.Text text])
            )
        ]
  | None -> null

let root (model : Model) (dispatch : Msg -> unit) =
  div
    [Style [Display "flex"] ]
    [
      board model dispatch
      chancesSummary "STAGE" model.StageWinChances
      chancesSummary "RACE" model.RaceWinChances
    ]
