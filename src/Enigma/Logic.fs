namespace Enigma

open System

module private AlphabetMappingFunctions = 
    let alphabet = String [|'A'..'Z'|]
    let toCharacter index = alphabet.[index]

    let shiftBy direction moveBy (letter:char) =
        match direction with
        | Forward -> (alphabet.IndexOf letter) + moveBy
        | Inverse -> (alphabet.IndexOf letter) - moveBy
        |> fun index -> (index + 26) % 26
        |> toCharacter

    let shiftUp = shiftBy Forward
    let shiftDown = shiftBy Inverse

    /// Rotates a given mapping by a specific amount
    let shiftMappingBy amount (mapping:char array) =
        if amount = 0 then mapping
        else Array.append mapping.[amount..] mapping.[..amount - 1]

module private Translation = 
    open AlphabetMappingFunctions
    
    let private applyWheelPosition (WheelPosition wheelPosition) (mapping:char array) =
        let wheelPosition = alphabet.IndexOf wheelPosition
        mapping
        |> shiftMappingBy wheelPosition
        |> Array.map (shiftDown wheelPosition)

    let private applyRingSetting (RingSetting ringSetting) (mapping:char array) =
        let ringSettingIndex = alphabet.IndexOf ringSetting
        mapping
        |> shiftMappingBy (alphabet.Length - ringSettingIndex)
        |> Array.map (shiftUp ringSettingIndex)

    let translateUsing (rotor, currentPosition) direction (letter:char) =
        let mapping = rotor.Mapping |> applyWheelPosition currentPosition |> applyRingSetting rotor.RingSetting
        match direction with
        | Forward -> mapping.[alphabet.IndexOf letter]
        | Inverse -> alphabet.[Array.IndexOf(mapping, letter)] 
    let reflectUsing (Reflector mapping) (letter:char) = mapping.[alphabet.IndexOf letter]
    let substituteUsing (PlugBoard plugboardMapping) (letter:char) =
        letter
        |> plugboardMapping.TryFind
        |> defaultArg <| letter

    let rotate (rotor, (WheelPosition currentPosition)) = 
        rotor, currentPosition 
               |> shiftUp 1
               |> WheelPosition

/// Contains standard Enigma rotors and reflectors.
module Components =
    let private createRotor (number, mapping, knockOns) = { ID = number; Mapping = mapping; KnockOns = knockOns |> List.map WheelPosition; RingSetting = RingSetting 'A' }
    let Rotor1 = createRotor (1, "EKMFLGDQVZNTOWYHXUSPAIBRCJ".ToCharArray(), [ 'Q' ])
    let Rotor2 = createRotor (2, "AJDKSIRUXBLHWTMCQGZNPYFVOE".ToCharArray(), [ 'E' ])
    let Rotor3 = createRotor (3, "BDFHJLCPRTXVZNYEIWGAKMUSQO".ToCharArray(), [ 'V' ])
    let Rotor4 = createRotor (4, "ESOVPZJAYQUIRHXLNFTGKDCMWB".ToCharArray(), [ 'J' ])
    let Rotor5 = createRotor (5, "VZBRGITYUPSDNHLXAWMJQOFECK".ToCharArray(), [ 'Z' ])
    let Rotor6 = createRotor (6, "JPGVOUMFYQBENHZRDKASXLICTW".ToCharArray(), [ 'Z'; 'M'])
    let Rotor7 = createRotor (7, "NZJHGRCXMYSWBOUFAIVLPEKQDT".ToCharArray(), [ 'Z'; 'M'])
    let Rotor8 = createRotor (8, "FKQHTLXOCBJSPDZRAMEWNIUYGV".ToCharArray(), [ 'Z'; 'M'])

    let Rotors = [ Rotor1; Rotor2; Rotor3; Rotor4; Rotor5; Rotor6; Rotor7; Rotor8 ]

    let ReflectorA = Reflector ("EJMZALYXVBWFCRQUONTSPIKHGD".ToCharArray())
    let ReflectorB = Reflector ("YRUHQSLDPXNGOKMIEBFZCWVJAT".ToCharArray())

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
        let isKnockOn (rotor, currentPosition) = rotor.KnockOns |> List.exists((=) currentPosition)
        match enigma with
        | enigma when enigma.Right |> isKnockOn -> { enigma with Middle = rotate enigma.Middle }
        | enigma when enigma.Middle |> isKnockOn -> { enigma with Left = rotate enigma.Left; Middle = rotate enigma.Middle }
        | _ -> enigma
    
    /// Translates an individual character and returns the updated Enigma state.
    let translateChar enigma letter =
        match Char.ToUpper letter with
        | other when (not << Char.IsLetter) other -> other, enigma
        | letter ->
            let enigma = { (enigma |> setAdjacentRotors) with Right = rotate enigma.Right }
            let result = letter |> doTranslation (enigma.Left, enigma.Middle, enigma.Right, enigma.Reflector, enigma.Plugboard)
            result, enigma

    /// Translates some text using the supplied enigma machine.
    let translate (text:String) enigma =
        (enigma, text.ToCharArray())
        ||> Array.mapFold translateChar
        |> fst
        |> String

[<AutoOpen>]
/// Contains builder functions to quickly create configured Enigma machines.
module Helpers =
    open Components

    /// An enigma machine using rotors 1-3 and reflector B with no plugboard.
    let defaultEnigma = 
        {   Left = Rotor1, WheelPosition 'A'
            Middle = Rotor2, WheelPosition 'A'
            Right = Rotor3, WheelPosition 'A'
            Reflector = ReflectorB
            Plugboard = PlugBoard Map.empty }

    /// Sets the rotors of the Enigma.
    let withRotors a b c enigma =
        { enigma with
            Left = a, snd enigma.Left
            Middle = b, snd enigma.Middle
            Right = c, snd enigma.Right }
    
    /// Adjusts the wheel positions of the Enigma.
    let withWheelPositions a b c enigma =
        { enigma with
            Left = fst enigma.Left, WheelPosition a
            Middle = fst enigma.Middle, WheelPosition b
            Right = fst enigma.Right, WheelPosition c }

    /// Adjusts the ring settings of the Enigma.
    let withRingSettings a b c enigma = 
        { enigma with
            Left = { fst enigma.Left with RingSetting = RingSetting a }, snd enigma.Left
            Middle = { fst enigma.Middle with RingSetting = RingSetting b }, snd enigma.Middle
            Right = { fst enigma.Right with RingSetting = RingSetting c }, snd enigma.Right }
    
    /// Adjusts the plugboard of the Enigma.       
    let withPlugBoard (mappings:string) enigma =
        { enigma with
            Plugboard =
                (Map.empty, mappings.Split ' ' |> Array.collect(fun pair -> [| pair.[0], pair.[1]; pair.[1], pair.[0] |]))
                ||> Array.fold(fun plugBoard -> plugBoard.Add)
                |> PlugBoard }


