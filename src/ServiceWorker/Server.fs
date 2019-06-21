module ServiceWorker.Server

open Fable.Core
open Fable.Import
open Fable.Import.JS
open Fable.Import.Browser
open Fable.Core.JsInterop
open Fable.Import.JS
open Fable
open Domain.Main

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
    printfn "receive message in worker _+_ %A" (JSON.stringify ev.data)
)

self.addEventListener_activate(fun activateEvent ->
    printfn "service worker activated, %A" activateEvent
    activateEvent.waitUntil(claim())
)
