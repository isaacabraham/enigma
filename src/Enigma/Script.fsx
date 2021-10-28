#load
    "Domain.fs"
    "Logic.fs"

open Enigma

//Please decrypt me :)
let enigma = { defaultEnigma with
                    Left = Components.Rotor3
                    Middle = Components.Rotor6
                    Right = Components.Rotor8 }
             |> withWheelPositions 'U' 'Z' 'V'
             |> withRingSettings 'A' 'H' 'M'
             |> withPlugBoard "AN EZ HK IJ LR MQ OT PV SW UX"

enigma |> translate "JDDDZQ DEEWT HQA"

let v = enigma |> translate "H"
enigma |> translate v

open System

let allLetters = [| 'A' .. 'Z' |]

module Fitness =
    let english = 0.0667
    let german = 0.0734
    let calculateCoincidence (text:string) =
        let text = text |> Seq.filter Char.IsLetter |> Seq.map Char.ToUpper |> Seq.toArray
        let N = float text.Length // text length
        let F =
            let sampleFrequencies =
                text
                |> Array.countBy id
                |> readOnlyDict

            allLetters
            |> Array.map (sampleFrequencies.TryGetValue >> function (false, _) -> 0 | true, x -> x)

        let firstBit = 1. / (N * (N - 1.))
        let secondBit = F |> Array.sumBy (fun value -> value * (value - 1)) |> float
        firstBit * secondBit

Fitness.calculateCoincidence "JDDDZQ DEEWT HQA"
let text = """Asynchronous updates usually follow a simple pattern: every asynchronous operation is coupled with a "start" event and a "finished" event. The start event doesn't need any parameter most of the time and is only responsible for issuing a command that eventually dispatches the finished event which carries the result of the operation."""
Fitness.calculateCoincidence text