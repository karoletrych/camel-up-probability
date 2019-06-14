module Home.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types

open System
open Elmish
open Elmish.React
open Fable.Import.React
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

open Fable.Import.Browser
open Fable.Import
open Fable.Import.React


// let makeDraggable (svg : Element) = 
//     let mutable dragged = null : Element
//     let mutable offset = None : (float* float) option

//     let getMousePosition (evt : MouseEvent) =
//       let svg = svg :?> SVGLocatable
//       let CTM = svg.getScreenCTM();
//       (
//         ((evt.clientX - CTM.e) / CTM.a),
//         ((evt.clientY - CTM.f) / CTM.d)
//       )

//     let startDrag = U2.Case1 (fun (evt : Event) -> 
//         let target = evt.target :?> Element
//         if (target).classList.contains "draggable"
//         then 
//             dragged <- target 
//             let (offsetx1, offsety1) = getMousePosition (evt :?> MouseEvent);
//             let (draggedPosXPercents, draggedPosYPercents) = 
//                 (JS.parseFloat (dragged.getAttribute("x"))), 
//                 (JS.parseFloat (dragged.getAttribute("y")))
//             let (draggedPosX, draggedPosY) = ((draggedPosXPercents/100.) * boardWidth,  (draggedPosYPercents/100.) * boardHeight)
//             let (offsetx2, ofssety2) = (offsetx1 - draggedPosX), (offsety1 - draggedPosY)
//             offset <- Some (offsetx2, ofssety2)

//     )
//     let drag = !^(fun (evt : Event) -> 
//       if dragged <> null then
//         evt.preventDefault();
//         let coordx, coordy = getMousePosition((evt :?> MouseEvent));
//         dragged.setAttribute("x", string (coordx - fst offset.Value));
//         dragged.setAttribute("y", string (coordy - snd offset.Value));
//     )
//     let endDrag = !^(fun evt -> 
//         dragged <- null)

//     svg.addEventListener("mousedown", startDrag);
//     svg.addEventListener("mousemove", drag);
//     svg.addEventListener("mouseup", endDrag);
//     svg.addEventListener("mouseleave", endDrag);

let drag (ev : DragEvent) = 
    ev.dataTransfer.setData("text", (ev.target :?> Element).id) |> ignore

let camelStack (camels : Camel list) fieldIndex =
    let (x, y) = coordsMapping |> Map.find fieldIndex
    camels 
    |> List.rev
    |> List.mapi (
        fun camelIndex camel ->
        [div
              [ 
                Style [
                    Width (sprintf "%d%%"(camelWidth))
                    Height (sprintf "%d%%"(camelHeight))
                    Left (sprintf "%d%%"(x * fieldSideHorizontal))
                    Top (sprintf "%d%%"((y+1) * fieldSideVertical - (camelIndex + 1) * camelHeight))
                    BackgroundColor (camelColor camel)
                    CSSProp.Position "absolute"
                    Border "1px solid black"
                    ]
                Draggable true
                OnDragStart drag
              ] []
        ])


let field fieldIndex =
    let field ((x,y),i) =
        [div
          [ 
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
          ] 
          [Fable.Helpers.React.HTMLNode.Text (string (i+1))]
        ]

    let coord = coordsMapping |> Map.find fieldIndex
    field (coord, fieldIndex)

let root model dispatch =
  div [ 
      Style [
          Width "500px"
          Height "500px"
          CSSProp.Position "relative"
          ]
  ]
   (
   ( [0..15] |> List.collect field) 
   @ 
   (camelStack [
       Camel.Blue; 
       Camel.Yellow;
       Camel.Orange;
       Camel.Green;
       Camel.White;
       ] 0 |> List.collect id)
       )
       
    

// let setup () = 
//     let elements = document.getElementsByClassName("camel-stack")
//     printfn "%A" elements.length
//     for i in [0..(int)elements.length-1] do
        // let e = elements.item (float i)
        // e.addEventListener("dragStart", drag)

// JS.setTimeout setup 100