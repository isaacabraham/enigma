namespace Enigma

open Enigma
open System

/// Contains standard Enigma rotors and reflectors.
module Configuration =
    let private createRotor (number, mapping: string, knockOns) = {
        ID = number
        Mapping = mapping.ToCharArray()
        Turnovers = knockOns
    }

    let Rotor1 = createRotor (1, "EKMFLGDQVZNTOWYHXUSPAIBRCJ", [ Turnover 16 ])

    let Rotor2 = createRotor (2, "AJDKSIRUXBLHWTMCQGZNPYFVOE", [ Turnover 4 ])

    let Rotor3 = createRotor (3, "BDFHJLCPRTXVZNYEIWGAKMUSQO", [ Turnover 21 ])

    let Rotor4 = createRotor (4, "ESOVPZJAYQUIRHXLNFTGKDCMWB", [ Turnover 9 ])

    let Rotor5 = createRotor (5, "VZBRGITYUPSDNHLXAWMJQOFECK", [ Turnover 25 ])

    let Rotor6 =
        createRotor (6, "JPGVOUMFYQBENHZRDKASXLICTW", [ Turnover 13; Turnover 25 ])

    let Rotor7 =
        createRotor (7, "NZJHGRCXMYSWBOUFAIVLPEKQDT", [ Turnover 13; Turnover 25 ])

    let Rotor8 =
        createRotor (8, "FKQHTLXOCBJSPDZRAMEWNIUYGV", [ Turnover 13; Turnover 25 ])

    let Rotors = [ Rotor1; Rotor2; Rotor3; Rotor4; Rotor5; Rotor6; Rotor7; Rotor8 ]

    let ReflectorA = Reflector("EJMZALYXVBWFCRQUONTSPIKHGD".ToCharArray())
    let ReflectorB = Reflector("YRUHQSLDPXNGOKMIEBFZCWVJAT".ToCharArray())

    let Reflectors = [ ReflectorA; ReflectorB ]

/// Contains builder functions to quickly create configured Enigma machines.
[<AutoOpen>]
module Helpers =
    open Configuration

    /// An enigma machine using rotors 1-3 and reflector B with no plugboard.
    let defaultEnigma = {
        Left = Rotor1
        Middle = Rotor2
        Right = Rotor3
        Reflector = ReflectorB
        Plugboard = PlugBoard Map.empty
    }

    /// Sets the rotors of the Enigma.
    let withRotors left middle right enigma = {
        enigma with
            Left = left
            Middle = middle
            Right = right
    }

    /// Adjusts the wheel positions of the Enigma.
    let withWheelPositions left middle right enigma = {
        enigma with
            Left = enigma.Left |> Rotor.withPosition left
            Middle = enigma.Middle |> Rotor.withPosition middle
            Right = enigma.Right |> Rotor.withPosition right
    }

    /// Adjusts the ring settings of the Enigma.
    let withRingSettings left middle right enigma = {
        enigma with
            Left = enigma.Left |> Rotor.withRingSetting left
            Middle = enigma.Middle |> Rotor.withRingSetting middle
            Right = enigma.Right |> Rotor.withRingSetting right
    }

    /// Adjusts the plugboard of the Enigma.
    let withPlugBoard (mappings: string) enigma = {
        enigma with
            Plugboard =
                (Map.empty,
                 mappings.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                 |> Array.collect (fun pair -> [| pair[0], pair[1]; pair[1], pair[0] |]))
                ||> Array.fold (fun plugBoard -> plugBoard.Add)
                |> PlugBoard
    }

    /// Moves the machine "forward" by a number of character presses.
    let moveForwardBy turns enigma =
        (enigma, [ 1..turns ])
        ||> List.fold (fun enigma _ -> Machine.translateChar enigma 'a' |> snd)
