module Test.Util

open Fable.Core
open Fable.Core.Testing

let [<Global>] describe (name: string) (f: unit -> unit) = jsNative
let [<Global>] it (msg: string) (f: unit -> unit) = jsNative

let assertEqual expected actual: unit =
    Assert.AreEqual(actual, expected)
