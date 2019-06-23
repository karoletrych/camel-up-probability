module Test.Main

open Test.Util
open Domain.Types

let mapSize = Domain.Types.Constants.fieldsCount + Domain.Types.Constants.maxRollDice

let run () =
    describe "BoardTests" <| fun () ->
        it "testMoveTwoCamelsOntoOtherTwo" <| fun () ->
          let map =
              Array.init mapSize
                (function
                | 2 -> CamelStack [Orange; Yellow] |> Some
                | 3 -> Plate PlusOne |> Some
                | 4 -> CamelStack [Blue] |> Some
                | 5 -> CamelStack [White; Green] |> Some
                | _ -> None)

          let (updatedMap, _) = map |> Domain.MoveCamel.applyRoll {Camel = Orange; Count = 3}

          let expectedMap =
              Array.init mapSize
                (function
                | 3 -> Plate PlusOne |> Some
                | 4 -> CamelStack [Blue] |> Some
                | 5 -> CamelStack [White; Green; Orange; Yellow] |> Some
                | _ -> None)

          assertEqual (Thoth.Json.Encode.Auto.toString(4, updatedMap)) (Thoth.Json.Encode.Auto.toString(4, expectedMap))

        it "testMoveCamelsOntoPlatePlusOneAdjucentToCamel" <| fun () ->
          let map =
              Array.init mapSize
                (function
                | 2 -> CamelStack [Orange; Yellow] |> Some
                | 3 -> Plate PlusOne |> Some
                | 4 -> CamelStack [Blue] |> Some
                | 5 -> CamelStack [White; Green] |> Some
                | _ -> None)

          let (updatedMap, _) = map |> Domain.MoveCamel.applyRoll {Camel = Yellow; Count = 1}

          let expectedMap =
              Array.init mapSize
                (function
                | 2 -> CamelStack [Orange] |> Some
                | 3 -> Plate PlusOne |> Some
                | 4 -> CamelStack [Blue; Yellow] |> Some
                | 5 -> CamelStack [White; Green] |> Some
                | _ -> None)

          assertEqual (Thoth.Json.Encode.Auto.toString(4, updatedMap)) (Thoth.Json.Encode.Auto.toString(4, expectedMap))

        it "testMoveCamelsOntoPlatePlusOneAdjucentToEmptyField" <| fun () ->
          let map =
              Array.init mapSize
                (function
                | 2 -> CamelStack [Orange; Yellow;] |> Some
                | 3 -> Plate PlusOne |> Some
                | 4 -> CamelStack [] |> Some
                | 5 -> CamelStack [White; Green; Blue] |> Some
                | _ -> None)

          let (updatedMap, _) = map |> Domain.MoveCamel.applyRoll {Camel = Yellow; Count = 1}

          let expectedMap =
              Array.init mapSize
                (function
                | 2 -> CamelStack [Orange] |> Some
                | 3 -> Plate PlusOne |> Some
                | 4 -> CamelStack [Yellow] |> Some
                | 5 -> CamelStack [White; Green; Blue] |> Some
                | _ -> None)
          assertEqual (Thoth.Json.Encode.Auto.toString(4, updatedMap)) (Thoth.Json.Encode.Auto.toString(4, expectedMap))

        it "testMoveCamelsOntoPlateMinusOneAdjucentToCamel" <| fun () ->
          let map =
              Array.init mapSize
                (function
                | 2 -> CamelStack [Orange; Yellow; White] |> Some
                | 4 -> CamelStack [Green; Blue] |> Some
                | 5 -> Plate MinusOne |> Some
                | _ -> None)

          let (updatedMap, _) = map |> Domain.MoveCamel.applyRoll {Camel = Yellow; Count = 3}

          let expectedMap =
              Array.init mapSize
                (function
                | 2 -> CamelStack [Orange] |> Some
                | 4 -> CamelStack [Yellow; White; Green; Blue] |> Some
                | 5 -> Plate MinusOne |> Some
                | _ -> None)

          assertEqual (Thoth.Json.Encode.Auto.toString(4, updatedMap)) (Thoth.Json.Encode.Auto.toString(4, expectedMap))

        it "testMoveCamelsOntoPlateMinusOneAdjucentToEmptyField" <| fun () ->
          let map =
              Array.init mapSize
                (function
                | 3 -> CamelStack [Orange; Yellow; White; Blue; Green] |> Some
                | 4 -> Plate MinusOne |> Some
                | _ -> None)

          let (updatedMap, _) = map |> Domain.MoveCamel.applyRoll {Camel = Orange; Count = 2}

          let expectedMap =
              Array.init mapSize
                (function
                | 3 -> CamelStack [Orange; Yellow; White; Blue; Green] |> Some
                | 4 -> Plate MinusOne |> Some
                | _ -> None)

          assertEqual (Thoth.Json.Encode.Auto.toString(4, updatedMap)) (Thoth.Json.Encode.Auto.toString(4, expectedMap))


run ()
