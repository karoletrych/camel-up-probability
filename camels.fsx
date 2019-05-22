
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
let allCamelsSet = [Yellow; Blue; Red; Green; White] |> Set.ofList

type Field =
| Tile of Tile
| CamelStack of Camel list

type Map = Field option array

type DiceRoll = {
    Count: int
    Camel: Camel
}

let allRollCombinations camelsLeft = 
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


let random = System.Random()

//TODO: maybe it should return seq<DiceRoll> instead of Seq<DiceRoll list>
let infiniteSimulatedRolls =
    let rec infiniteSimulatedRolls (acc : DiceRoll list) (remainingCamels : Camel Set) = seq {
        match remainingCamels with
        | empty when empty.IsEmpty -> 
            yield acc
            yield! infiniteSimulatedRolls [] allCamelsSet
        | remainingCamels -> 
            let randomCamelIndex = random.Next(remainingCamels |> Set.count)
            let randomCamel = (remainingCamels |> Set.toList).[randomCamelIndex] // TODO: optimize
            let count = random.Next(1,4);
            let roll = {Camel = randomCamel; Count = count}
            yield! infiniteSimulatedRolls (roll::acc) (remainingCamels |> Set.remove randomCamel)
    }
    infiniteSimulatedRolls [] allCamelsSet



let setElement key newEl = 
    Array.mapi (fun k v -> if k = key then newEl else v)

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
               |> setElement camelMapIndex 
                    fieldToLeave

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
                |> setElement newCamelMapIndex (Some (CamelStack newCamelStack))
            | None -> 
                mapWithRemovedCamels 
                |> setElement newCamelMapIndex (Some (CamelStack camelsToMove))
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

let winnerPercentages totalGames = 
    List.map (fun (camel, gamesWon) -> 
        (camel, (float gamesWon) / (float totalGames))
    )
    >> List.sortByDescending snd

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