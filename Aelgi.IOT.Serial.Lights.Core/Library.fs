namespace Aelgi.IOT.Serial.Lights.Core

open System.Drawing

type WriteAction =
    | Color of Color
    | Reset
    | Show
