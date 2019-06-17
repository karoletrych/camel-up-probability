module Home.Main
open Home.Types
open Home
open System.Collections.Generic

let takeWhilePlusOne predicate (s:seq<_>) = 
  let rec loop (en:IEnumerator<_>) = seq {
    if en.MoveNext() then
      yield en.Current
      if predicate en.Current then
        yield! loop en }

  seq { use en = s.GetEnumerator()
        yield! loop en }


let playUntilFinish initialState applyRoll (rollSequence : DiceRoll seq) = 
  rollSequence
  |> Seq.scan (fun state inp -> 
       state |> (fun state -> applyRoll (fst state) inp)) (initialState, 0)
  |> takeWhilePlusOne (fun (_, finalIndex) -> finalIndex < 16)
  |> Seq.last |> (fun (result, _) -> result)

let winner (map : Map) : Camel = 
    let (Some (CamelStack lastNonEmptyStack)) =
        map
        |> Array.findBack (
            function 
            | Some (CamelStack []) -> false
            | Some (CamelStack _) -> true
            | _ -> false) 
    lastNonEmptyStack |> List.head
    

let winnerPercentages totalGames = 
    List.map (fun (camel, gamesWon) -> 
        (camel, (float gamesWon) / (float totalGames))
    )
    >> List.sortByDescending snd

let winnerCounts map camelsLeft = 
    RollSequences.allRollCombinations camelsLeft
    |> Seq.map (playUntilFinish map MoveCamel.applyRoll)
    |> Seq.map winner
    |> Seq.countBy id
    |> Seq.toList

let stageWinChances map camelsLeft =
  let winnerCounts = winnerCounts map camelsLeft
  let totalGames = winnerCounts |> List.sumBy snd
  winnerPercentages totalGames winnerCounts


// let randomSimulatedWinnerCounts =
//     RollSequences.infiniteSimulatedRolls
//     |> Seq.take 10_000_000
//     |> Seq.map (playGame initialState)
//     |> Seq.map winner
//     |> Seq.countBy id
//     |> Seq.toList

// randomSimulatedWinnerCounts
// |> winnerPercentages 10_000_000

// RollSequences.infiniteSimulatedRolls
// |> Seq.take 1_000_000
// |> Seq.countBy (fun x -> x |> List.head |> fun x -> x.Count)


//TODO: sequence length = 16
// rollCombinations allCamels


// let print (maps : Map list ) =
//     for m in maps do
//         printfn "" 
//         for x in m do
//             printfn "%A" x

// let test = 
//     rollCombinations allCamels
//     |> Seq.map (playGame initialState)
//     |> Seq.toList

// print test

// let winners =
//     test
//     |> List.map winner