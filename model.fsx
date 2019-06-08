type Camel =
| Yellow
| Blue
| Red
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
