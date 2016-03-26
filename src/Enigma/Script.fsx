#load "Domain.fs"
#load "Logic.fs"
open Enigma

//Please decrypt me :)
let enigma = ({ defaultEnigma with 
                    Reflector = Components.ReflectorB
                    Left = Components.Rotor3, WheelPosition 'U'
                    Middle = Components.Rotor6, WheelPosition 'Z'
                    Right = Components.Rotor8, WheelPosition 'V' }
                |> withRingSettings 'A' 'H' 'M'
                |> withPlugBoard "AN EZ HK IJ LR MQ OT PV SW UX")

enigma |> translate "JDDDZQ DEEWT HQA"