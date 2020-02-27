module Aelgi.IOT.Serial.Lights.LightsVisualizer

open Aelgi.IOT.Serial.Lights.Core

let handleAction (action: WriteAction) =
    match action with
    | Reset -> printfn "RESET"
    | Show -> printfn "SHOW"
    | Color c -> printfn "COLOR (%d, %d, %d)" c.R c.G c.B