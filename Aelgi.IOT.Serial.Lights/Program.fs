// Learn more about F# at http://fsharp.org

open System
open System.Diagnostics
open System.Drawing
open System.Threading
open Aelgi.IOT.Serial.Lights.Core
open Aelgi.IOT.Serial.Lights
open Aelgi.IOT.Serial.Lights.Animations

type CurrentAnimation =
    | HueWave
    | Fireplace
    | Blue
    | Purple
    | CustomHue of int
    
let processInputToAnimation (c: char) =
    match c with
    | 'h' | '1' -> HueWave |> Some
    | 'f' | '2' -> Fireplace |> Some
    | 'b' | '0' -> Blue |> Some
    | 'p' -> Purple |> Some
    | 'c' -> Console.ReadLine() |> int |> CustomHue |> Some
    | _ -> None

let rec initialSerial () =
    printf "Loading available serial ports..."
    let ports = SerialWriter.getAvailablePorts ()
    printfn "\rAvailable Serial Ports:"
    ports |> List.iteri (printfn " - %d) %s")
    
    let pressedNumber =
        (Console.Read ()) - (int '0')
        
    let selectedPort =
        ports
        |> List.tryItem pressedNumber
        
    match selectedPort with
    | Some p ->
        printfn "Selected: %s" p
        SerialWriter.connectToPort p
    | None ->
        printfn "Failed to find port!"
        initialSerial()

let initialVisualisation stripCount =
    let mutable state = LightsVisualizer.initializeVisualizer stripCount
    
    let render action =
        state <- LightsVisualizer.handleAction state action
        ()
        
    render
    
let showColor (color: Color) (stripCount: int) =
    [Reset]@[
        for _ = 1 to stripCount do
            yield color |> Color
    ]@[Show]
    
let blue = showColor Color.SkyBlue
let purple = showColor Color.Purple
let customHue stripCount (hue: int) = stripCount |> (hsv hue 1. 1. |> showColor)

[<EntryPoint>]
let main argv =
    let stripCount = 19
    let isDevelopment =
        "DOTNET_ENVIRONMENT"
        |> Environment.GetEnvironmentVariable
        |> function
            | "Development" -> true
            | _ -> false
          
    let stopWatch = Stopwatch.StartNew()
    let frameLimiter () =
        let targetFPS = int64 20
        let targetMillisPerFrame = (int64 1000) / targetFPS
        let elapsed = stopWatch.ElapsedMilliseconds
        
        let timeToPause = targetMillisPerFrame - elapsed
        if timeToPause > (int64 5) then
            timeToPause |> int |> Thread.Sleep
        
        stopWatch.Restart()
        ()
            
    let writeAction =
        match isDevelopment with
        | true ->
            let handler = initialVisualisation stripCount
            handler
        | false ->
            let serial = initialSerial()
            SerialWriter.handleAction serial
    let writer =
        List.iter writeAction
        
    let hueRender =
        let mutable state = HueWave.initialState
        let render () =
            let (actions, newState) = HueWave.render state
            state <- newState
            actions
        render
        
    let fireplaceRender =
        let mutable state = Fireplace.initialState 16 stripCount
        let render () =
            let (actions, newState) = Fireplace.render state
            state <- newState
            actions
        render
        
    let mutable currentAnimation = Fireplace
        
    while true do
        let frames =
            match currentAnimation with
            | HueWave -> hueRender()
            | Fireplace -> fireplaceRender()
            | Blue -> blue stripCount
            | Purple -> purple stripCount
            | CustomHue h -> customHue stripCount h
            
        frames |> writer
        
        let nextAnimation =
            match Console.KeyAvailable with
            | true -> Console.ReadKey().KeyChar |> Some
            | false -> None
            |> Option.bind (processInputToAnimation)
        match nextAnimation with
        | Some a ->
            currentAnimation <- a
            ()
        | None -> ()
            
        
        frameLimiter ()
    
    0 // return an integer exit code
