module App.Types

open Global

type Msg =
    | HomeMsg of Home.Types.Msg

type Model =
    { CurrentPage: Page
      Home: Home.Types.Model }
