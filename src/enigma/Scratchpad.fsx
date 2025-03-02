#load "Domain.fs" "Configuration.fs"
open Enigma

//Please decrypt me :)
let machine =
    {
        defaultEnigma with
            Left = Rotor3
            Middle = Rotor6
            Right = Rotor8
            Debug = true
    }
    |> withRotorPositions 'U' 'Z' 'V'
    |> withRingSettings 'A' 'H' 'M'
    |> withPlugBoard "AN EZ HK IJ LR MQ OT PV SW UX"

machine |> Machine.translate "JDDDZQ DEEWT HQA"

defaultEnigma |> Machine.moveForwardBy 101
