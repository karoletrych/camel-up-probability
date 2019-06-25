module ServiceWorker.Server

open Fable.Core
open Fable.Import
open Fable.Import.JS
open Fable.Import.Browser
open Fable.Core.JsInterop
open Fable.Import.JS
open Fable
open Domain.Main
open Api

let self = self :?> ServiceWorker

[<Emit("self.skipWaiting()")>]
let skipWaiting ():Promise<obj option> = jsNative

[<Emit("self.clients.claim()")>]
let claim (): Promise<obj option> = jsNative

self.addEventListener_install(fun installEvent ->
    printfn "service worker installed, %A" installEvent
    installEvent.waitUntil(skipWaiting())
)

self.addEventListener_message(fun ev ->
  let message = Thoth.Json.Decode.Auto.unsafeFromString<Api.Request>(string ev.data)
  let result =
      match message.RequestContent with
      | CalculateRaceWinChancesCommand {Map = map; DicesLeft = dicesLeft} -> Domain.Main.raceWinChances map dicesLeft
      | CalculateStageWinChancesCommand {Map = map; DicesLeft = dicesLeft} -> Domain.Main.stageWinChances map dicesLeft

  let responseDto = {ResponseContent= result; Id = message.Id}
  let response = Thoth.Json.Encode.Auto.toString(0, responseDto)

  let ports = ev.ports :?> Browser.MessagePort array
  ports.[0].postMessage(response)
)

self.addEventListener_activate(fun activateEvent ->
    printfn "service worker activated, %A" activateEvent
    activateEvent.waitUntil(claim())
)
