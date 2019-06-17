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

type DiceRoll = {
    Count : int
    Camel : Camel
}

type Field =
| Tile of Tile
| CamelStack of Camel list

type Map = Field option array

type Model = {
    Map : Map
    DicesLeft : Camel list
    StageWinChances : ((Camel * float) list) option
}

type DropPlace =
    | OnTopOfCamel of Camel
    | OnField of int

type Msg =
    | CamelDropped of Camel * DropPlace
