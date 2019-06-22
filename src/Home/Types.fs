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
    | ResetDices
    | MarkDiceAsUsed of Camel
    | RollDice of Camel * int
    | RaceWinChancesComputed of ((Camel * float) list)
    | StageWinChancesComputed of ((Camel * float) list)
    | ComputationError

