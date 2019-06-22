module ServiceWorkerClient
open Fable.Core
open Fable.Import
open Fable.PowerPack
open System
open ServiceWorker.Api
open Fable.Import

let serializeCommand command =
    let message = {
      Id = Guid.NewGuid();
      RequestContent = command
    }
    let json = Thoth.Json.Encode.Auto.toString(0, message)
    json

[<Emit("('serviceWorker' in navigator)")>]
let hasSWSupport () = jsNative

let postMsg (command : Command) =
  Promise.create (fun resolve reject ->
    let messageChannel = Browser.MessageChannel.Create()

    messageChannel.port1.onmessage <- (fun ev ->
      let result = Thoth.Json.Decode.Auto.unsafeFromString<Response>(string ev.data)
      resolve(result.ResponseContent)
    )

    Browser.navigator.serviceWorker.controller
      |> Option.iter(fun worker ->
        let message = serializeCommand command
        worker.postMessage(message, [|messageChannel.port2|])
      )
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

