module Domain.Types

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

module Constants =
    let fieldsCount = 16
    let maxRollDice = 3
