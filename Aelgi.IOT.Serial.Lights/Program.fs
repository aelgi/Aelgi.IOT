// Learn more about F# at http://fsharp.org

open System
open Aelgi.IOT.Serial.Lights

let initialSerial () =
    let ports = SerialWriter.getAvailablePorts ()
    printfn "Available Serial Ports:"
    ports |> List.iter (printfn " - %s")
    
    ()

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
