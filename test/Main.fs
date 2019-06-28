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
                | 2 -> CamelStack [Yellow; Orange;] |> Some
                | 3 -> Plate PlusOne |> Some
                | 4 -> CamelStack [Blue] |> Some
                | 5 -> CamelStack [Green; White;] |> Some
                | _ -> None)

          let (updatedMap, _) = map |> Domain.MoveCamel.applyRoll {Camel = Orange; Count = 3}

          let expectedMap =
              Array.init mapSize
                (function
                | 3 -> Plate PlusOne |> Some
                | 4 -> CamelStack [Blue] |> Some
                | 5 -> CamelStack [Yellow;Orange;Green; White;] |> Some
                | _ -> None)

          assertEqual (Thoth.Json.Encode.Auto.toString(4, expectedMap)) (Thoth.Json.Encode.Auto.toString(4, updatedMap))

        it "testMoveCamelsOntoPlatePlusOneAdjucentToCamel" <| fun () ->
          let map =
              Array.init mapSize
                (function
                | 2 -> CamelStack [Yellow; Orange; ] |> Some
                | 3 -> Plate PlusOne |> Some
                | 4 -> CamelStack [Blue] |> Some
                | 5 -> CamelStack [Green; White; ] |> Some
                | _ -> None)

          let (updatedMap, _) = map |> Domain.MoveCamel.applyRoll {Camel = Yellow; Count = 1}

          let expectedMap =
              Array.init mapSize
                (function
                | 2 -> CamelStack [Orange] |> Some
                | 3 -> Plate PlusOne |> Some
                | 4 -> CamelStack [Yellow; Blue; ] |> Some
                | 5 -> CamelStack [Green; White; ] |> Some
                | _ -> None)

          assertEqual (Thoth.Json.Encode.Auto.toString(4, expectedMap)) (Thoth.Json.Encode.Auto.toString(4, updatedMap))

        it "testMoveCamelsOntoPlatePlusOneAdjucentToEmptyField" <| fun () ->
          let map =
              Array.init mapSize
                (function
                | 2 -> CamelStack [Yellow; Orange; ] |> Some
                | 3 -> Plate PlusOne |> Some
                | 4 -> CamelStack [] |> Some
                | 5 -> CamelStack [Blue; Green; White;  ] |> Some
                | _ -> None)

          let (updatedMap, _) = map |> Domain.MoveCamel.applyRoll {Camel = Yellow; Count = 1}

          let expectedMap =
              Array.init mapSize
                (function
                | 2 -> CamelStack [Orange] |> Some
                | 3 -> Plate PlusOne |> Some
                | 4 -> CamelStack [Yellow] |> Some
                | 5 -> CamelStack [Blue; Green; White;] |> Some
                | _ -> None)
          assertEqual (Thoth.Json.Encode.Auto.toString(4, expectedMap)) (Thoth.Json.Encode.Auto.toString(4, updatedMap))

        it "testMoveCamelsOntoPlateMinusOneAdjucentToCamel" <| fun () ->
          let map =
              Array.init mapSize
                (function
                | 2 -> CamelStack [White; Yellow; Orange;  ] |> Some
                | 4 -> CamelStack [Blue; Green; ] |> Some
                | 5 -> Plate MinusOne |> Some
                | _ -> None)

          let (updatedMap, _) = map |> Domain.MoveCamel.applyRoll {Camel = Yellow; Count = 3}

          let expectedMap =
              Array.init mapSize
                (function
                | 2 -> CamelStack [Orange] |> Some
                | 4 -> CamelStack [Blue; Green; White; Yellow;] |> Some
                | 5 -> Plate MinusOne |> Some
                | _ -> None)

          assertEqual (Thoth.Json.Encode.Auto.toString(4, expectedMap)) (Thoth.Json.Encode.Auto.toString(4, updatedMap))

        it "testMoveCamelsOntoPlateMinusOneAdjucentToEmptyField" <| fun () ->
          let map =
              Array.init mapSize
                (function
                | 2 -> CamelStack [Green; Blue; White; Yellow; Orange;] |> Some
                | 4 -> Plate MinusOne |> Some
                | _ -> None)

          let (updatedMap, _) = map |> Domain.MoveCamel.applyRoll {Camel = Orange; Count = 2}

          let expectedMap =
              Array.init mapSize
                (function
                | 3 -> CamelStack [Green; Blue; White; Yellow; Orange;] |> Some
                | 4 -> Plate MinusOne |> Some
                | _ -> None)

          assertEqual (Thoth.Json.Encode.Auto.toString(4, expectedMap)) (Thoth.Json.Encode.Auto.toString(4, updatedMap))
        it "testMoveCamelsOntoPlateMinusOneAdjucentToCamel" <| fun () ->
          let map =
              Array.init mapSize
                (function
                | 3 -> CamelStack [Green; Blue; White; Yellow; Orange;] |> Some
                | 4 -> Plate MinusOne |> Some
                | _ -> None)

          let (updatedMap, _) = map |> Domain.MoveCamel.applyRoll {Camel = White; Count = 1}

          let expectedMap =
              Array.init mapSize
                (function
                | 3 -> CamelStack [Yellow; Orange; Green; Blue; White;] |> Some
                | 4 -> Plate MinusOne |> Some
                | _ -> None)

          assertEqual (Thoth.Json.Encode.Auto.toString(4, expectedMap)) (Thoth.Json.Encode.Auto.toString(4, updatedMap))


run ()
