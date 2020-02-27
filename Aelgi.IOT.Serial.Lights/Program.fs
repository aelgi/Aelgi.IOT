// Learn more about F# at http://fsharp.org

open System
open Aelgi.IOT.Serial.Lights

let rec initialSerial () =
    let ports = SerialWriter.getAvailablePorts ()
    printfn "Available Serial Ports:"
    ports |> List.iteri (printfn " - %d) %s")
    
    let pressedNumber =
        (Console.Read ()) - (int '0')
        
    let selectedPort =
        ports
        |> List.tryItem pressedNumber
        
    match selectedPort with
    | Some p ->
        printfn "Selected: %s" p
    | None ->
        printfn "Failed to find port!"
        initialSerial()
        

[<EntryPoint>]
let main argv =
    let isDevelopment =
        "DOTNET_ENVIRONMENT"
        |> Environment.GetEnvironmentVariable
        |> function
            | "Development" -> true
            | _ -> false
            
    let serial = initialSerial ()
    
    printfn "Hello World from F#!"
    0 // return an integer exit code
