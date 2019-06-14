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
