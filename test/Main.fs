module Test.Main

open Test.Util

let run () =
    describe "tests" <| fun () ->
        it "asserts 1 is 1" <| fun () ->
            assertEqual 1 1

run ()
