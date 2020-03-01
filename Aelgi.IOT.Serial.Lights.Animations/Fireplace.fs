module Aelgi.IOT.Serial.Lights.Animations.Fireplace

open System
open Aelgi.IOT.Serial.Lights.Core

type FireEvolution = Growing | Dying

type Fire =
    {
        Location: int
        Strength: double
        Value: double
        Evolution: FireEvolution
    }

type FireplaceState =
    {
        MaxFires: int
        CurrentFires: Fire list
        StripLength: int
    }
    
let private rnd = new Random()
let initialState (numFlames: int) (stripLength: int) =
    {
        FireplaceState.CurrentFires = []
        MaxFires = numFlames
        StripLength = stripLength
    }
    
let rec newLocation (locs: int list) (posMax: int) =
    let location = rnd.Next posMax
    locs
    |> List.tryFind ((=) location)
    |> function
        | Some _ -> newLocation locs posMax
        | None -> location

let newRandomFlame (state: FireplaceState) (stripLength: int) =
    let location = newLocation (state.CurrentFires |> List.map (fun x -> x.Location)) stripLength
    let strength = rnd.NextDouble() / 15.
    {
        Fire.Location = location
        Strength = strength
        Value = 0.
        Evolution = Growing
    }

let spawnOnShortage (state: FireplaceState) =
    state.CurrentFires
    |> List.length
    |> function
        | length when length < state.MaxFires ->
            { state with CurrentFires = (newRandomFlame state state.StripLength)::state.CurrentFires }
        | _ -> state
        
let killOnDeath (state: FireplaceState) =
    state.CurrentFires
    |> List.filter (fun x ->
        match x.Evolution with
        | Dying -> x.Value > 0.1
        | Growing -> true)
    |> (fun x -> { state with CurrentFires = x })
            
let handleFireStrength (fire: Fire) =
    let capValue =
        function
            | i when i > 1. -> 1.
            | i when i < 0. -> 0.
            | i -> i
            
    let newValue =
        fire.Value
        |> match fire.Evolution with
            | Growing -> (+)
            | Dying -> (-)
        |> (fun x -> x fire.Strength)
        |> capValue
        
    let newEvolution =
        match newValue with
        | i when i > 0.99 -> Dying |> Some
        | _ -> None
        |> Option.defaultValue fire.Evolution
            
    { fire with Value = newValue; Evolution = newEvolution }
            
let handleStrengths (state: FireplaceState) =
    let fires =
        state.CurrentFires
        |> List.map handleFireStrength
    { state with CurrentFires = fires }
    
let generateColor (fire: Fire) =
    let hue =
        (fire.Value * 30.)
        |> int
    hsv hue 1. fire.Value

let render (state: FireplaceState) =
    let newState =
        state
        |> spawnOnShortage
        |> handleStrengths
        |> killOnDeath
        
    let colors =
        List.init state.StripLength (fun pos ->
            newState.CurrentFires
            |> List.tryFind (fun x -> x.Location = pos)
            |> Option.map (generateColor)
            |> Option.defaultValue (hsv 0 1. 0.))
        |> List.map (Color)
    
    let colorCommands = Show::colors
    (colorCommands, newState)