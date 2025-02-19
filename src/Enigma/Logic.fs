namespace Enigma

open System

module private AlphabetMappingFunctions =
    let alphabet = String [| 'A' .. 'Z' |]
    let toCharacter index = alphabet.[index]

    let shiftBy direction moveBy (letter: char) =
        match direction with
        | Forward -> (alphabet.IndexOf letter) + moveBy
        | Inverse -> (alphabet.IndexOf letter) - moveBy
        |> fun index -> (index + 26) % 26
        |> toCharacter

    let shiftUp = shiftBy Forward
    let shiftDown = shiftBy Inverse

    /// Rotates a given mapping by a specific amount
    let shiftMappingBy amount (mapping: char array) =
        if amount = 0 then
            mapping
        else
            Array.append mapping.[amount..] mapping.[.. amount - 1]

module private Translation =
    open AlphabetMappingFunctions

    let applyRingSetting (RingSetting ringSetting) (mapping: char array) =
        let ringSettingIndex = alphabet.IndexOf ringSetting

        mapping
        |> shiftMappingBy (alphabet.Length - ringSettingIndex)
        |> Array.map (shiftUp ringSettingIndex)

    let translateUsing rotor direction (letter: char) =
        match direction with
        | Forward -> rotor.Mapping.[alphabet.IndexOf letter]
        | Inverse -> alphabet.[Array.IndexOf(rotor.Mapping, letter)]

    let reflectUsing (Reflector mapping) (letter: char) = mapping.[alphabet.IndexOf letter]

    let substituteUsing (PlugBoard plugboardMapping) (letter: char) =
        letter |> plugboardMapping.TryFind |> defaultArg <| letter

    /// Rotates the rotor by 1 notch.
    let rotate (rotor: Rotor) =
        let rotateRotorBy distance (mapping: char array) =
            mapping |> shiftMappingBy distance |> Array.map (shiftDown distance)

        {
            rotor with
                Mapping = rotor.Mapping |> rotateRotorBy 1
                KnockOns = rotor.KnockOns |> List.map (fun (KnockOn ko) -> KnockOn((ko - 1 + 26) % 26))
        }

/// Contains standard Enigma rotors and reflectors.
module Components =
    let private createRotor (number, mapping, knockOns) = {
        ID = number
        Mapping = mapping
        KnockOns = knockOns
    }

    let Rotor1 =
        createRotor (1, "EKMFLGDQVZNTOWYHXUSPAIBRCJ".ToCharArray(), [ KnockOn 16 ])

    let Rotor2 =
        createRotor (2, "AJDKSIRUXBLHWTMCQGZNPYFVOE".ToCharArray(), [ KnockOn 4 ])

    let Rotor3 =
        createRotor (3, "BDFHJLCPRTXVZNYEIWGAKMUSQO".ToCharArray(), [ KnockOn 21 ])

    let Rotor4 =
        createRotor (4, "ESOVPZJAYQUIRHXLNFTGKDCMWB".ToCharArray(), [ KnockOn 9 ])

    let Rotor5 =
        createRotor (5, "VZBRGITYUPSDNHLXAWMJQOFECK".ToCharArray(), [ KnockOn 25 ])

    let Rotor6 =
        createRotor (6, "JPGVOUMFYQBENHZRDKASXLICTW".ToCharArray(), [ KnockOn 13; KnockOn 25 ])

    let Rotor7 =
        createRotor (7, "NZJHGRCXMYSWBOUFAIVLPEKQDT".ToCharArray(), [ KnockOn 13; KnockOn 25 ])

    let Rotor8 =
        createRotor (8, "FKQHTLXOCBJSPDZRAMEWNIUYGV".ToCharArray(), [ KnockOn 13; KnockOn 25 ])

    let Rotors = [ Rotor1; Rotor2; Rotor3; Rotor4; Rotor5; Rotor6; Rotor7; Rotor8 ]

    let ReflectorA = Reflector("EJMZALYXVBWFCRQUONTSPIKHGD".ToCharArray())
    let ReflectorB = Reflector("YRUHQSLDPXNGOKMIEBFZCWVJAT".ToCharArray())

    let Reflectors = [ ReflectorA; ReflectorB ]

/// Contains high-level operations to access Enigma.
[<AutoOpen>]
module Operations =
    open Translation

    let private doTranslation (left, middle, right, reflector, plugboard) =
        substituteUsing plugboard
        >> translateUsing right Forward
        >> translateUsing middle Forward
        >> translateUsing left Forward
        >> reflectUsing reflector
        >> translateUsing left Inverse
        >> translateUsing middle Inverse
        >> translateUsing right Inverse
        >> substituteUsing plugboard

    let private setAdjacentRotors enigma =
        let isKnockOn rotor =
            rotor.KnockOns |> List.contains (KnockOn 0)

        match enigma with
        | enigma when enigma.Right |> isKnockOn -> {
            enigma with
                Middle = rotate enigma.Middle
          }
        | enigma when enigma.Middle |> isKnockOn -> {
            enigma with
                Left = rotate enigma.Left
                Middle = rotate enigma.Middle
          }
        | _ -> enigma

    /// Translates an individual character and returns the updated Enigma state.
    let translateChar enigma letter =
        match Char.ToUpper letter with
        | other when (not << Char.IsLetter) other -> other, enigma
        | letter ->
            let enigma = {
                (enigma |> setAdjacentRotors) with
                    Right = rotate enigma.Right
            }

            let result =
                letter
                |> doTranslation (enigma.Left, enigma.Middle, enigma.Right, enigma.Reflector, enigma.Plugboard)

            result, enigma

    /// Translates some text using the supplied enigma machine.
    let translate (text: String) enigma =
        (enigma, text.ToCharArray()) ||> Array.mapFold translateChar |> fst |> String

[<AutoOpen>]
/// Contains builder functions to quickly create configured Enigma machines.
module Helpers =
    open Components

    /// An enigma machine using rotors 1-3 and reflector B with no plugboard.
    let defaultEnigma = {
        Left = Rotor1
        Middle = Rotor2
        Right = Rotor3
        Reflector = ReflectorB
        Plugboard = PlugBoard Map.empty
    }

    /// Sets the rotors of the Enigma.
    let withRotors a b c enigma = {
        enigma with
            Left = a
            Middle = b
            Right = c
    }

    /// Adjusts the wheel positions of the Enigma.
    let withWheelPositions (a: char) (b: char) (c: char) enigma =
        let setRotorToPosition (newWheelPosition: char) rotor =
            let skipDistance = AlphabetMappingFunctions.alphabet.IndexOf newWheelPosition

            (rotor, [ 1..skipDistance ])
            ||> List.fold (fun rotor _ -> Translation.rotate rotor)

        {
            enigma with
                Left = enigma.Left |> setRotorToPosition a
                Middle = enigma.Middle |> setRotorToPosition b
                Right = enigma.Right |> setRotorToPosition c
        }

    /// Adjusts the ring settings of the Enigma.
    let withRingSettings a b c enigma = {
        enigma with
            Left = {
                enigma.Left with
                    Mapping = Translation.applyRingSetting (RingSetting a) enigma.Left.Mapping
            }
            Middle = {
                enigma.Middle with
                    Mapping = Translation.applyRingSetting (RingSetting b) enigma.Middle.Mapping
            }
            Right = {
                enigma.Right with
                    Mapping = Translation.applyRingSetting (RingSetting c) enigma.Right.Mapping
            }
    }

    /// Adjusts the plugboard of the Enigma.
    let withPlugBoard (mappings: string) enigma = {
        enigma with
            Plugboard =
                (Map.empty,
                 mappings.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
                 |> Array.collect (fun pair -> [| pair.[0], pair.[1]; pair.[1], pair.[0] |]))
                ||> Array.fold (fun plugBoard -> plugBoard.Add)
                |> PlugBoard
    }

    /// Moves the machine "forward" by a number of character presses.
    let moveForwardBy turns enigma =
        (enigma, [ 1..turns ])
        ||> List.fold (fun enigma _ -> translateChar enigma 'a' |> snd)
