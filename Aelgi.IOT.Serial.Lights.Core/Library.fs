namespace Aelgi.IOT.Serial.Lights.Core

open System
open System.Drawing

type WriteAction =
    | Color of Color
    | Reset
    | Show

[<AutoOpen>]
module Helpers =
    let hsvDouble (h: double) (s: double) (v: double) =
        if (v <= 0.) then System.Drawing.Color.FromArgb(0, 0, 0)
        elif (s <= 0.) then v |> int |> (fun x -> (x, x, x)) |> System.Drawing.Color.FromArgb
        else
            let hf = h / 60.
            let i = hf |> Math.Floor |> int
            
            let f = hf - (i |> double)
            let pv = v * (1. - s)
            let qv = v * (1. - s * f)
            let tv = v * (1. - s * (1. - f))
            
            let (r, g, b) =
                match i with
                | 0 | 6 -> (v, tv, pv)
                | 1 -> (qv, v, pv)
                | 2 -> (pv, v, tv)
                | 3 -> (pv, qv, v)
                | 4 -> (tv, pv, v)
                | 5 | -1 -> (v, pv, qv)
                | _ -> (v, v, v)
                
            let clamp =
                function
                    | i when i < 0. -> 0
                    | i when i > 255. -> 255
                    | i -> i |> int
                    
            (r |> clamp, g |> clamp, b |> clamp) |> System.Drawing.Color.FromArgb
            
    let hsv hue sat value =
        hsvDouble (hue |> double) (sat |> double) (value |> double)