let allCamels = [Yellow; Blue; Red; Green; White]
let allCamelsSet = [Yellow; Blue; Red; Green; White] |> Set.ofList


let setElement key newEl = 
    Array.mapi (fun k v -> if k = key then newEl else v)

let winner (map : Map) : Camel = 
    let (Some (CamelStack lastNonEmptyStack)) =
        map
        |> Array.findBack (
            function 
            | Some (CamelStack _) -> true
            | _ -> false) 
    lastNonEmptyStack |> List.head
    

let playGame = List.fold applyRoll

let initialState : Map = 
    Array.init 16
        (function
        | 0 -> CamelStack allCamels |> Some
        | _ -> None)

let winnerPercentages totalGames = 
    List.map (fun (camel, gamesWon) -> 
        (camel, (float gamesWon) / (float totalGames))
    )
    >> List.sortByDescending snd

allRollCombinations allCamels
|> Seq.length

let winnerCounts = 
    allRollCombinations allCamels
    |> Seq.map (playGame initialState)
    |> Seq.map winner
    |> Seq.countBy id
    |> Seq.toList

winnerCounts
|> winnerPercentages (winnerCounts |> List.sumBy snd)

let randomSimulatedWinnerCounts =
    infiniteSimulatedRolls
    |> Seq.take 10_000_000
    |> Seq.map (playGame initialState)
    |> Seq.map winner
    |> Seq.countBy id
    |> Seq.toList

randomSimulatedWinnerCounts
|> winnerPercentages 10_000_000

infiniteSimulatedRolls
|> Seq.take 1_000_000
|> Seq.countBy (fun x -> x |> List.head |> fun x -> x.Count)


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