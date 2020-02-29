﻿// Learn more about F# at http://fsharp.org

open System
open System.Diagnostics
open System.Drawing
open System.Threading
open Aelgi.IOT.Serial.Lights.Core
open Aelgi.IOT.Serial.Lights

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
    
let generateOpening (stripCount: int) =
    [Reset]@[
        for _ = 1 to stripCount do
            yield Color.SkyBlue |> Color
    ]@[Show]

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
        let targetFPS = int64 30
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
        
    generateOpening stripCount
    |> writer
    
    while true do
        frameLimiter ()
    
    0 // return an integer exit code
