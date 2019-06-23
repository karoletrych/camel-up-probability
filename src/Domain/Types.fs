module Domain.Types

type Camel =
| Yellow
| Blue
| Orange
| Green
| White

type Plate =
| PlusOne
| MinusOne

type DiceRoll = {
    Count : int
    Camel : Camel
}

type Field =
| Plate of Plate
| CamelStack of Camel list

type Map = Field option array

module Constants =
    let fieldsCount = 16
    let maxRollDice = 3
