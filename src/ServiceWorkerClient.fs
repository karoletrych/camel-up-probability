module ServiceWorkerClient
open Fable.Core
open Fable.Import
open Fable.PowerPack


[<Emit("('serviceWorker' in navigator)")>]
let hasSWSupport () = jsNative

let postMsg () =
  Browser.navigator.serviceWorker.controller
    |> Option.iter(fun ctr ->
        JS.setTimeout (fun () ->
            ctr.postMessage("hello from page.")
        ) 2000
        |> ignore
    )
let install () =
    if hasSWSupport() then
        Browser.window.addEventListener_load(fun ev ->
            Browser.navigator.serviceWorker.register("/sw.js")
            |> Promise.map (fun registration ->
                printfn "registration successful %A" registration
            )
            |> Promise.catchEnd (fun err ->
                printfn "Failed to register service worker, %A" err
            )
    )

