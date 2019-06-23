module Home.Types
open Domain.Types


type Model = {
    Map : Map
    DicesLeft : Camel list
    StageWinChances : ((Camel * float) list) option
    RaceWinChances : ((Camel * float) list) option
}

type DropPlace =
    | OnTopOfCamel of Camel
    | OnField of int

type Msg =
    | CamelDropped of Camel * DropPlace
    // plates
    | ExistingPlateDropped of int * int
    | NewPlateDroppedOnBoard of Plate * int
    | FlipPlate of int
    | RemovePlates

    // dices
    | ResetDices
    | MarkDiceAsUsed of Camel
    | RollDice of Camel * int

    // async computations
    | RaceWinChancesComputed of ((Camel * float) list)
    | StageWinChancesComputed of ((Camel * float) list)
    | ComputationError

