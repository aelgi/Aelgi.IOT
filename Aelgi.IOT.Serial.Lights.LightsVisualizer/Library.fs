module Aelgi.IOT.Serial.Lights.LightsVisualizer

open System
open System.Drawing
open Aelgi.IOT.Serial.Lights.Core
open Pastel

type VisualizerState =
    {
        StripLength: int
        StripColors: Color list
    }

let initializeVisualizer (stripLength: int) =
    { VisualizerState.StripColors = []; StripLength = stripLength }
    
let manageState (state: VisualizerState) =
    let stripLength = List.length state.StripColors
    if stripLength > state.StripLength then
        let newColors = state.StripColors |> List.take state.StripLength
        { state with StripColors = newColors }
    else state

let handleAction (state: VisualizerState) (action: WriteAction) =
    match action with
    | Reset ->
        initializeVisualizer state.StripLength
    | Show ->
        state.StripColors
        |> List.iter (" ".PastelBg >> printf "%s")
        printf "\r"
        
        manageState state
    | Color c ->
        manageState { state with VisualizerState.StripColors = (c::state.StripColors) }