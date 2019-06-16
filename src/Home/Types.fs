module Home.Types

type Camel =
| Yellow
| Blue
| Orange
| Green
| White

type Tile = 
| Oasis
| Mirage

type Field =
| Tile of Tile
| CamelStack of Camel list

type Model = Field option array

type Msg =
    | CamelDropped of Camel * int

