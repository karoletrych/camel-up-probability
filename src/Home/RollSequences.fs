module Home.RollSequences
open Home.Types

let allCamelsSet = [Yellow; Blue; Orange; Green; White] |> Set.ofList


let allRollCombinations dicesLeft =
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
                    let newDicesLeft =
                        possibleCamels
                        |> List.where (fun c -> c <> roll.Camel)
                    yield! rollCombinations (roll::acc) newDicesLeft
        }
    rollCombinations [] dicesLeft


let random = System.Random()

let infiniteSimulatedRolls dicesLeft =
    let rec infiniteSimulatedRolls (acc : DiceRoll list) (remainingDices : Camel Set) = seq {
        match remainingDices with
        | empty when empty.IsEmpty ->
            yield acc
            yield! infiniteSimulatedRolls [] allCamelsSet
        | remainingDices ->
            let randomCamelIndex = random.Next(remainingDices |> Set.count)
            let randomCamel = (remainingDices |> Set.toList).[randomCamelIndex] // TODO: optimize
            let count = random.Next(1, Constants.maxRollDice + 1);
            let roll = {Camel = randomCamel; Count = count}
            yield! infiniteSimulatedRolls (roll::acc) (remainingDices |> Set.remove randomCamel)
    }
    infiniteSimulatedRolls [] (Set.ofList dicesLeft)
    |> Seq.collect id
