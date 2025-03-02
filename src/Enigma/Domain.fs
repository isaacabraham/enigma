namespace Enigma

type AlphabetMapping = char array
type RingSetting = char
type Turnover = Turnover of int
type PlugBoard = PlugBoard of Map<char, char>

type Direction =
    | Up
    | Down

/// Low-level functions that move characters around the alphabet.
module Alphabet =
    open System
    let alphabet = String [| 'A' .. 'Z' |]

    let shiftBy direction moveBy (letter: char) =
        let index =
            let letterIndex = alphabet.IndexOf letter

            match direction with
            | Up -> letterIndex + moveBy
            | Down -> letterIndex - moveBy

        let index = (index + 26) % 26
        alphabet[index]

    let shiftUp = shiftBy Up
    let shiftDown = shiftBy Down

    /// Rotates a given mapping by a specific amount
    let offsetBy amount (mapping: AlphabetMapping) =
        match amount with
        | 0 -> mapping
        | amount -> Array.append mapping[amount..] mapping[.. amount - 1]

type Rotor = {
    ID: int
    Mapping: AlphabetMapping
    Turnovers: Turnover list
}

type Reflector = Reflector of AlphabetMapping

type Machine = {
    Left: Rotor
    Middle: Rotor
    Right: Rotor
    Reflector: Reflector
    Plugboard: PlugBoard
}

module Rotor =
    open Alphabet
    open System

    /// Rotates the rotor by 1 notch.
    let rotate rotor =
        let rotateBy distance (mapping: AlphabetMapping) =
            mapping |> offsetBy distance |> Array.map (shiftDown distance)

        {
            rotor with
                Mapping = rotor.Mapping |> rotateBy 1
                Turnovers = rotor.Turnovers |> List.map (fun (Turnover v) -> Turnover((v - 1 + 26) % 26))
        }

    /// Sets the rotor to a specific position.
    let withPosition (newWheelPosition: char) rotor =
        let skipDistance = alphabet.IndexOf newWheelPosition

        (rotor, [ 1..skipDistance ]) ||> List.fold (fun rotor _ -> rotate rotor)

    /// Applies a ring setting to a rotor.
    let withRingSetting (ringSetting: RingSetting) rotor =
        let ringSettingIndex = alphabet.IndexOf ringSetting

        {
            rotor with
                Mapping =
                    rotor.Mapping
                    |> offsetBy (alphabet.Length - ringSettingIndex)
                    |> Array.map (shiftUp ringSettingIndex)
        }

    /// Translates a single character using the supplied rotor in a specific direction.
    let translate rotor direction (letter: char) =
        match direction with
        | Up -> rotor.Mapping[alphabet.IndexOf letter]
        | Down -> alphabet[Array.IndexOf(rotor.Mapping, letter)]

module Reflector =
    open Alphabet

    let translate (Reflector mapping) (letter: char) = mapping[alphabet.IndexOf letter]

module PlugBoard =
    let translate (PlugBoard mapping) letter =
        letter |> mapping.TryFind |> Option.defaultValue letter

/// Contains high-level operations to operate an Enigma machine.
module Machine =
    open System

    let private doTranslation (leftRotor, middleRotor, rightRotor, reflector, plugboard) =
        PlugBoard.translate plugboard
        >> Rotor.translate rightRotor Up
        >> Rotor.translate middleRotor Up
        >> Rotor.translate leftRotor Up
        >> Reflector.translate reflector
        >> Rotor.translate leftRotor Down
        >> Rotor.translate middleRotor Down
        >> Rotor.translate rightRotor Down
        >> PlugBoard.translate plugboard

    let private setAdjacentRotors machine =
        let isTurnover rotor =
            rotor.Turnovers |> List.contains (Turnover 0)

        match machine with
        | machine when machine.Right |> isTurnover -> {
            machine with
                Middle = Rotor.rotate machine.Middle
          }
        | machine when machine.Middle |> isTurnover -> {
            machine with
                Left = Rotor.rotate machine.Left
                Middle = Rotor.rotate machine.Middle
          }
        | _ -> machine

    let (|Alpha|NonAlpha|) c =
        if Char.IsLetter c then Alpha(Char.ToUpper c) else NonAlpha

    /// Translates an individual character and returns the updated Enigma state.
    let translateChar machine letter =
        match letter with
        | NonAlpha -> letter, machine
        | Alpha letter ->
            let machine = {
                setAdjacentRotors machine with
                    Right = Rotor.rotate machine.Right
            }

            let result =
                letter
                |> doTranslation (machine.Left, machine.Middle, machine.Right, machine.Reflector, machine.Plugboard)

            result, machine

    /// Translates some text using the supplied enigma machine.
    let translate (text: String) machine =
        (machine, text.ToCharArray()) ||> Array.mapFold translateChar |> fst |> String
