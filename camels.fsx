type Camel =
| Yellow
| Blue
| Red
| Green
| White

type Tile =
| Oasis
| Mirage

let allCamels = [Yellow; Blue; Red; Green; White]
type Position = int

type Field =
| Tile of Tile
| CamelStack of Camel list

type Map = Field option array

type DiceRoll = {
    Count: int
    Camel: Camel
}

let rollCombinations camelsLeft = 
    let rec rollCombinations 
        (acc : DiceRoll list) 
        (possibleCamels: Camel List) 
        : DiceRoll list seq = 
        seq {
            let possibleRollsNow = 
                [for i in [1;2;3] do
                    for camel in possibleCamels do 
                        yield {Camel = camel; Count = i}
                ]
            match possibleRollsNow with
            | [] -> yield acc
            | rolls ->
                for roll in rolls do
                    let newCamelsLeft = 
                        possibleCamels 
                        |> List.where (fun c -> c <> roll.Camel)            
                    yield! rollCombinations (roll::acc) newCamelsLeft 
        }
    rollCombinations [] camelsLeft

let setElement key s = 
    Array.map (fun (k, v) -> if k = key then (k, s) else (k, v))



let applyRoll (map : Map) (roll : DiceRoll) : Map =
    let {Count = rollNumber; Camel = camel} = roll
    // printfn "map: %A" map
    // printfn "roll: %A" roll
    
    let (camelMapIndex, Some (CamelStack camelStack)) = 
        map 
        |> Array.indexed
        |> Array.find (
            function 
            | (_, Some (CamelStack s)) -> s |> List.contains camel
            | _ -> false
        )

    let camelStackIndex = camelStack |> List.findIndex ((=) camel)

    // printfn "camel stack: %A" camelStack
    // printfn "index: %A" camelStackIndex

    let (camelsToMove, camelsToStay) = 
        camelStack 
        |> List.splitAt (camelStackIndex + 1)

    // printfn "move: %A" camelsToMove
    // printfn "stay: %A" camelsToStay

    let newCamelMapIndex = camelMapIndex + rollNumber;
    let fieldToLeave = 
        match camelsToStay with
        | [] -> None
        | camels -> (Some (CamelStack camels))

    let mapWithRemovedCamels = 
               map     
               |> Array.indexed
               |> setElement camelMapIndex 
                    fieldToLeave
               |> Array.map snd

    // printfn "map: %A" mapWithRemovedCamels

    let newField = 
        map 
        |> Array.tryItem newCamelMapIndex

    // printfn "new field: %A" newField

    let map =
        match newField with
        | None -> 
            map
        | Some field -> 
            match field with
            | Some (Tile t) -> failwith "TODO: Tile"
            | Some (CamelStack camels) -> 
                let newCamelStack =  camelsToMove @ camels
                mapWithRemovedCamels 
                |> Array.indexed 
                |> setElement newCamelMapIndex (Some (CamelStack newCamelStack))
                |> Array.map snd
            | None -> 
                mapWithRemovedCamels 
                |> Array.indexed 
                |> setElement newCamelMapIndex (Some (CamelStack camelsToMove))
                |> Array.map snd
    // printfn "map at end: %A" map
    // printfn "" 
    map        

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


let winnerCounts = 
    rollCombinations allCamels
    |> Seq.map (playGame initialState)
    |> Seq.map winner
    |> Seq.countBy id
    |> Seq.toList

let totalGames = 
    winnerCounts 
    |> List.sumBy snd

let winnerPercentages =
    winnerCounts
    |> List.map (fun (camel, gamesWon) -> 
        (camel, (float gamesWon) / (float totalGames))
    )

allCamels

allCamels
totalGames
winnerCounts 
|> List.sortByDescending snd

winnerPercentages
|> List.sortByDescending snd

// rollCombinations allCamels
// |> Seq.length

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