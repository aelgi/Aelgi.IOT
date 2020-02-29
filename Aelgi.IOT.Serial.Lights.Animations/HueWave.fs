module Aelgi.IOT.Serial.Lights.Animations.HueWave

open System.Drawing
open Aelgi.IOT.Serial.Lights.Core

type HueWaveState =
    {
        currentHue: int
    }
    
let initialState = { HueWaveState.currentHue = 0 }

let render (state: HueWaveState) =
    let currentHue =
        if state.currentHue > 360 then 0
        else state.currentHue + 5
        
    let nextState = { state with currentHue = currentHue }
    
    let nextColorCommand = hsv currentHue 100 100 |> Color
    let showCommand = Show
    
    (showCommand::[nextColorCommand], nextState)