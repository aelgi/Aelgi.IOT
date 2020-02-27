// Learn more about F# at http://fsharp.org

open System
open System.Drawing
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
        
let initialVisualisation () =
    printfn "Visualisation mode enabled..."
        
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
            
    let writeAction =
        match isDevelopment with
        | true ->
            initialVisualisation()
            LightsVisualizer.handleAction
        | false ->
            let serial = initialSerial()
            SerialWriter.handleAction serial
    let writer =
        List.iter writeAction
        
    generateOpening stripCount
    |> writer
    
    printfn "Hello World from F#!"
    0 // return an integer exit code
