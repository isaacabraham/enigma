#load "Domain.fs" "Logic.fs"
open Enigma

//Please decrypt me :)
let machine =
    {
        defaultEnigma with
            Left = Configuration.Rotor3
            Middle = Configuration.Rotor6
            Right = Configuration.Rotor8
    }
    |> withWheelPositions 'U' 'Z' 'V'
    |> withRingSettings 'A' 'H' 'M'
    |> withPlugBoard "AN EZ HK IJ LR MQ OT PV SW UX"

machine |> Machine.translate "JDDDZQ DEEWT HQA"

let m = defaultEnigma

m.Left
m.Left |> Rotor.withRingSetting 'B'

Rotor.translate (m.Left |> Rotor.withRingSetting 'B') Up 'A'
