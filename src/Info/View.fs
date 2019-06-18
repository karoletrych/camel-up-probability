module Info.View

open Fable.Helpers.React
open Fable.Helpers.React.Props

let root =
  div
    [ ClassName "content" ]
    [ h1
        [ ]
        [ str "About page" ]
      p
        [ ]
        [ str "This app calculates probabilities of each camel winning in the game Camel Up." ]
      p
        [ ]
        [ str "Built with Fable + Elmish + React." ]
    ]
