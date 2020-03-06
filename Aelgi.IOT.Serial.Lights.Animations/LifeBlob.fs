module Aelgi.IOT.Serial.Lights.Animations.LifeBlob

open System
open Aelgi.IOT.Serial.Lights.Core

// Lifeforce simulation
// Once per frame a lifeforce can look left and right, the result of looking is if the lifeforce is bigger or smaller than them and is of the same team
// A lifeforce may move one unit per turn, if they move onto another lifeforce, one of the two actions may occur:
//     1. if the life force is smaller then it is "eaten" all of its remaining energy is given to the "consuming" lifeforce
//     2. if the life force is bigger then it "eats" the smaller lifeforce and gets all of the lifeforces energy
// A lifeforce of the same team cannot move onto the same unit as another
// A lifeforce will loose energy with time and if it reaches 0 energy then it "dies"
// If a lifeforce gains more than 1 unit of energy then it is able to spawn a new lifeforce near by with it's "excess energy"
// If there are not enough lifeforces then a new lifeforce is spawned with energy of 0.3

type Team =
    | Red
    | Orange
    | Blue
    | Purple
    | Green
    
type LifeForceEnergy = LifeForceEnergy of float

type LifeForce =
    {
        Id: Guid
        Energy: LifeForceEnergy
        Team: Team
        Position: int
    }
    
type LifeBlobState =
    {
        MaxLifeForces: int
        CurrentLifeForces: LifeForce list
        StripLength: int
    }
    
let private rnd = new Random()
let initialState (numForces: int) (stripLength: int) =
    {
        LifeBlobState.CurrentLifeForces = []
        MaxLifeForces = numForces
        StripLength = stripLength
    }
    
let rec newLocation (locs: int list) (posMax: int) =
    let location = rnd.Next posMax
    locs
    |> List.tryFind ((=) location)
    |> function
        | Some _ -> newLocation locs posMax
        | None -> location
        
let generateStrength () =
    0.3 |> LifeForceEnergy
    
let newRandomLifeForce (state: LifeBlobState) (stripLength: int) =
    let location = newLocation (state.CurrentLifeForces |> List.map (fun x -> x.Position)) stripLength
    let team =
        match rnd.Next 5 with
        | 0 -> Red
        | 1 -> Orange
        | 2 -> Blue
        | 3 -> Purple
        | _ -> Green
    {
        LifeForce.Energy = generateStrength()
        Position = location
        Team = team
        Id = Guid.NewGuid()
    }
    
let spawnOnShortage (state: LifeBlobState) =
    state.CurrentLifeForces
    |> List.length
    |> function
        | length when length < state.MaxLifeForces ->
            { state with CurrentLifeForces = (newRandomLifeForce state state.StripLength)::state.CurrentLifeForces }
        | _ -> state
        
let isStillAlive (LifeForceEnergy x) =
    x > 0.
        
let killOnDeath (state: LifeBlobState) =
    state.CurrentLifeForces
    |> List.filter (fun x -> isStillAlive x.Energy)
    |> (fun x -> { state with CurrentLifeForces = x })
    
let reduceLifeForce (x: LifeForce) =
    match x.Energy with
    | LifeForceEnergy e ->
        match e - 0.01 with
        | res when res > 0. -> res
        | _ -> 0.
        |> LifeForceEnergy
        
type ClosestLifeForce =
    | Bigger
    | Smaller
    | SameTeam
    | NotFound
    
let compareEnergy (LifeForceEnergy a) (LifeForceEnergy b) =
    if (b > a) then Smaller
    else Bigger
    
let combineLifeForce (LifeForceEnergy a) (LifeForceEnergy b) =
    match a + b with
    | i when i >= 1. -> 1.
    | i -> i
    |> LifeForceEnergy
    
let rec findLifeForm (lifes: LifeForce list) (operation: int -> int) (lifeForm: LifeForce) (stripLength: int) (position: int) =
    let newPosition = operation position
    
    if newPosition >= stripLength || newPosition < 0 then NotFound
    else
        lifes
        |> List.tryFind (fun x -> x.Position = newPosition)
        |> function
            | Some f ->
                if f.Team = lifeForm.Team then SameTeam
                else
                    compareEnergy f.Energy lifeForm.Energy
            | None ->
                findLifeForm lifes operation lifeForm stripLength newPosition
        
let moveToAdvantage (state: LifeBlobState) (x: LifeForce) =
    let leftForm = findLifeForm state.CurrentLifeForces ((+) -1) x state.StripLength x.Position
    let rightForm = findLifeForm state.CurrentLifeForces ((+) 1) x state.StripLength x.Position
    
    match (leftForm, rightForm) with
    | (Smaller, _) -> x.Position - 1
    | (_, Smaller) -> x.Position + 1
    | (Bigger, Bigger) -> x.Position
    | (Bigger, _) -> x.Position + 1
    | (_, Bigger) -> x.Position - 1
    | (SameTeam, SameTeam) -> x.Position
    | (SameTeam, _) -> x.Position + 1
    | (_, SameTeam) -> x.Position - 1
    | (NotFound, NotFound) -> x.Position
    |> function
        | i when i >= state.StripLength -> state.StripLength - 1
        | i when i < 0 -> 0
        | i -> i
        
let eating (state: LifeBlobState) (x: LifeForce) =
    let existing =
        state.CurrentLifeForces
        |> List.tryFind (fun y -> y.Position = x.Position && not (y.Id.Equals x.Id ))
        
    existing
    |> Option.bind (fun exist ->
        match compareEnergy x.Energy exist.Energy with
        | Bigger -> combineLifeForce x.Energy exist.Energy |> Some
        | Smaller -> 0. |> LifeForceEnergy |> Some
        | _ -> None)
    |> Option.defaultValue x.Energy
    
let handleLifeForces (state: LifeBlobState) =
    let lifeForces =
        state.CurrentLifeForces
        |> List.map (fun x -> { x with Energy = reduceLifeForce x; Position = moveToAdvantage state x })
        |> List.map (fun x -> { x with Energy = eating state x })
        
    { state with CurrentLifeForces = lifeForces }
    
let getIntensity (LifeForceEnergy x) =
    x
    
let generateColor (life: LifeForce) =
    let hue =
        match life.Team with
        | Red -> 0
        | Orange -> 30
        | Blue -> 200
        | Purple -> 260
        | Green -> 120
    hsv hue 1. (getIntensity life.Energy)
    
let render (state: LifeBlobState) =
    let newState =
        state
        |> spawnOnShortage
        |> handleLifeForces
        |> killOnDeath
        
    let colors =
        List.init state.StripLength (fun pos ->
            newState.CurrentLifeForces
            |> List.tryFind (fun x -> x.Position = pos)
            |> Option.map (generateColor)
            |> Option.defaultValue (hsv 0 1. 0.))
        |> List.map (Color)
    
    (Show::colors, newState)