module Home.Types

type Model = string

type Msg =
    | ChangeStr of string

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

type Map = Field option array

type DiceRoll = {
    Count: int
    Camel: Camel
}
