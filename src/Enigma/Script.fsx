#load "Domain.fs"
open Enigma
#load "Logic.fs"
open Enigma

//Please decrypt me :)
let enigma = ({ defaultEnigma
                     with 
                          Reflector = Components.ReflectorB
                          Left = Components.Rotor3, WheelPosition 'A'
                          Middle = Components.Rotor6, WheelPosition 'A'
                          Right = Components.Rotor8, WheelPosition 'A' }
                |> withWheelPositions 'U' 'Z' 'V'
                |> withRingSettings 'A' 'H' 'M'
                |> withPlugBoard "AN EZ HK IJ LR MQ OT PV SW UX")

enigma |> translate "JDDDZQ DEEWT HQA"
