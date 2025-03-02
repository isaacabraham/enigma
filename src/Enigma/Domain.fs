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
    Offset: int
    RingSetting: RingSetting
}

type Reflector = Reflector of AlphabetMapping

type Machine = {
    Left: Rotor
    Middle: Rotor
    Right: Rotor
    Reflector: Reflector
    Plugboard: PlugBoard
    Debug: bool
}

module Rotor =
    open Alphabet
    open System

    /// Rotates the rotor by 1 notch.
    let rotate rotor = {
        rotor with
            Mapping = rotor.Mapping |> offsetBy 1 |> Array.map (shiftDown 1)
            Turnovers = rotor.Turnovers |> List.map (fun (Turnover v) -> Turnover((v - 1 + 26) % 26))
            Offset = (rotor.Offset + 1) % 26
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
                RingSetting = ringSetting
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

    let debugger title translator input =
        let output = translator input
        printfn $"{title}: {input} -> {output}"
        output

    let translationPipeline machine =
        let debugger = if machine.Debug then debugger else fun _ -> id

        debugger "Plugboard" (PlugBoard.translate machine.Plugboard)
        >> debugger "Right" (Rotor.translate machine.Right Up)
        >> debugger "Middle" (Rotor.translate machine.Middle Up)
        >> debugger "Left" (Rotor.translate machine.Left Up)
        >> debugger "Reflector" (Reflector.translate machine.Reflector)
        >> debugger "Left" (Rotor.translate machine.Left Down)
        >> debugger "Middle" (Rotor.translate machine.Middle Down)
        >> debugger "Right" (Rotor.translate machine.Right Down)
        >> debugger "Plugboard" (PlugBoard.translate machine.Plugboard)

    let (|RightTurnover|MiddleTurnover|Neither|) machine =
        let isTurnover rotor =
            rotor.Turnovers |> List.contains (Turnover 0)

        if machine.Right |> isTurnover then RightTurnover
        elif machine.Middle |> isTurnover then MiddleTurnover
        else Neither

    /// Processes the "turnovers" of the rotors.
    let turnover machine =
        match machine with
        | RightTurnover -> {
            machine with
                Middle = Rotor.rotate machine.Middle
          }
        | MiddleTurnover -> {
            machine with
                Middle = Rotor.rotate machine.Middle
                Left = Rotor.rotate machine.Left
          }
        | Neither -> machine

    let (|Alpha|NonAlpha|) c =
        if Char.IsLetter c then Alpha(Char.ToUpper c) else NonAlpha

    /// Translates an individual character and returns the updated Enigma state.
    let translateChar machine letter =
        match letter with
        | NonAlpha -> letter, machine
        | Alpha letter ->
            let machine = {
                turnover machine with
                    Right = Rotor.rotate machine.Right
            }

            let result = letter |> translationPipeline machine

            result, machine

    /// Translates some text using the supplied enigma machine.
    let translate (text: String) machine =
        (machine, text.ToCharArray()) ||> Array.mapFold translateChar |> fst |> String

    /// Moves the machine "forward" by a number of character presses.
    let moveForwardBy turns enigma =
        (enigma, [ 1..turns ])
        ||> List.fold (fun enigma _ -> translateChar enigma 'a' |> snd)
