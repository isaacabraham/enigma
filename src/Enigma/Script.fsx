#load "Domain.fs"
#load "Logic.fs"
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

// Sample of a crude brute force attack on Engima
// Only works without a custom plugboard!
#load @"../../.paket/load/main.group.fsx"
#load "Bombe.fs"

let someSecretText = "CGZJT HRYVXHEYY"
let crib = "HELLO"

let (Some (tries, machine)) = Bombe.tryFindSolution someSecretText crib ignore

machine
|> translate "CGZJT HRYVXHEYY"