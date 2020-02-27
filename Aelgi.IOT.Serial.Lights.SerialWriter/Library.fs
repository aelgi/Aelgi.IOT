module Aelgi.IOT.Serial.Lights.SerialWriter

open System.IO.Ports
open Aelgi.IOT.Serial.Lights.Core

type SerialConnection =
    {
        name: string
        port: SerialPort
    }

let getAvailablePorts () =
    SerialPort.GetPortNames ()
    |> Array.toList
    
let connectToPort (port: string) =
    let serial = new SerialPort(port, 115200)
    serial.Open()
    serial.Write("rs")
    { SerialConnection.name = port; port = serial }

let handleAction (connection: SerialConnection) (action: WriteAction) =
    match action with
    | Reset -> connection.port.Write("r")
    | Show -> connection.port.Write("s")
    | Color c ->
        ([| byte 'c'; c.R; c.G; c.B |], 0, 3)
        |> connection.port.Write
