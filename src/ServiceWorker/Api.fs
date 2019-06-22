module ServiceWorker.Api
open Domain.Types
open System

type CalculateStageWinChancesCommand = {
  Map : Map
  DicesLeft : Camel list
}

type CalculateRaceWinChancesCommand = {
  Map : Map
  DicesLeft : Camel list
}

type Command =
| CalculateRaceWinChancesCommand of CalculateRaceWinChancesCommand
| CalculateStageWinChancesCommand of CalculateStageWinChancesCommand

type Result = (Domain.Types.Camel * float) list

type Request = {
  Id: Guid
  RequestContent : Command
}

type Response = {
  Id: Guid
  ResponseContent : Result
}
