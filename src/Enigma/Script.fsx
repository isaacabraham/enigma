#load "Domain.fs" "Logic.fs"
open Enigma

//Please decrypt me :)
let enigma =
    {
        defaultEnigma with
            Left = Components.Rotor3
            Middle = Components.Rotor6
            Right = Components.Rotor8
    }
    |> withWheelPositions 'U' 'Z' 'V'
    |> withRingSettings 'A' 'H' 'M'
    |> withPlugBoard "AN EZ HK IJ LR MQ OT PV SW UX"

enigma |> translate "JDDDZQ DEEWT HQA"
