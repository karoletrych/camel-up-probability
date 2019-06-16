module Home.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types
open Fable.Import

open System
open Elmish
open Elmish.React
open Types
type SVG = SVGAttr

let fieldSideHorizontal = 100 / 5
let fieldSideVertical = 100 / 5

let camelHeight = fieldSideVertical / 5
let camelWidth = fieldSideHorizontal

let boardWidth = 100.
let boardHeight = 100.

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
| Camel.Blue -> "blue"
| Camel.Orange -> "orange"
| Camel.Green -> "green"
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

let allowDrop(ev : DragEvent) = 
  ev.preventDefault()


let camelStack (camels : Camel list) fieldIndex dispatch =
    let (x, y) = coordsMapping |> Map.find fieldIndex
    
    let dragStart (ev : DragEvent) = 
      let id = (ev.target :?> Fable.Import.Browser.Element).id
      ev.dataTransfer.setData("text", id) |> ignore

    let drop(ev : DragEvent) = 
      ev.preventDefault()
      JS.debugger ()
      let targetCamelElement = (ev.target :?> Fable.Import.Browser.Element)
      let targetCamel = camelFromId targetCamelElement.id
      let camelId = ev.dataTransfer.getData("text")
      let camel = camelFromId camelId
      (CamelDropped (camel, fieldIndex)) |> dispatch

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
                    Left (sprintf "%d%%"(x * fieldSideHorizontal))
                    Top (sprintf "%d%%"((y+1) * fieldSideVertical - (camelIndex + 1) * camelHeight))
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
      (CamelDropped (camel, fieldIndex)) |> dispatch


    let field ((x,y),i) =
        [div
          [ 
              Id (sprintf "field-%d" i)
              Style [
                  Width (sprintf "%d%%"(fieldSideHorizontal))
                  Height (sprintf "%d%%"(fieldSideVertical))
                  Left (sprintf "%d%%"(x*fieldSideHorizontal))
                  Top (sprintf "%d%%"(y*fieldSideVertical))
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

let root model (dispatch : Msg -> unit) =
  div [ 
      Style [
          Width "500px"
          Height "500px"
          CSSProp.Position "relative"
          ]
  ]
   (
   ( [0..15] |> List.collect (field dispatch)) 
   @ 
   (camelStack [
       Camel.Blue; 
       Camel.Yellow;
       Camel.Orange;
       Camel.Green;
       Camel.White;
       ] 0 dispatch |> List.collect id)
       )
       
    

// let setup () = 
//     let elements = document.getElementsByClassName("camel-stack")
//     printfn "%A" elements.length
//     for i in [0..(int)elements.length-1] do
        // let e = elements.item (float i)
        // e.addEventListener("dragStart", drag)

// JS.setTimeout setup 100