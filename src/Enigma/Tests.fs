module Enigma.Tests

open Configuration
open FsCheck
open System
open Expecto

let unitTests =
    testList "Basic Unit Tests" [
        test "Should translate a message that only needs the right rotor to advance" {
            Expect.equal
                (defaultEnigma
                 |> withRotorPositions 'A' 'B' 'C'
                 |> Machine.translate "AEFAEJXXBNXYJTY")
                "CONGRATULATIONS"
                "Does not match"
        }
        test "Should translate a message with rotor turnover" {
            Expect.equal
                (defaultEnigma
                 |> withRotorPositions 'A' 'B' 'R'
                 |> Machine.translate "MABE KGZXSG")
                "TURN MIDDLE"
                "Does not match"
        }
        test "Should translate a message with double stepping" {
            Expect.equal
                (defaultEnigma
                 |> withRotorPositions 'A' 'D' 'S'
                 |> Machine.translate "RZFOG FYHPL")
                "TURNS THREE"
                "Does not match"
        }
        test "Should translate a message with ring settings" {
            Expect.equal
                (defaultEnigma
                 |> withRotorPositions 'X' 'Y' 'Z'
                 |> withRingSettings 'J' 'N' 'U'
                 |> Machine.translate "QKTP EBZIUK")
                "GOOD RESULT"
                "Does not match"
        }
        test "Should translate a message with custom rotors" {
            Expect.equal
                (defaultEnigma
                 |> withRotors Rotor2 Rotor3 Rotor1
                 |> withRotorPositions 'X' 'Y' 'Z'
                 |> withRingSettings 'J' 'N' 'U'
                 |> Machine.translate "WMUOMJ YRLFLA")
                "CUSTOM ROTORS"
                "Does not match"
        }
        test "Should translate a message with a plugboard" {
            Expect.equal
                (defaultEnigma
                 |> withRotorPositions 'V' 'Q' 'Q'
                 |> withRingSettings 'J' 'N' 'U'
                 |> withPlugBoard "AP BR CM FZ GJ IL NT OV QS WX"
                 |> Machine.translate "HABHV HL YDFN ADZY")
                "THATS IT WELL DONE"
                "Does not match"
        }
    ]

let enigmaTests =
    testList "Enigma Tests" [
        test "Enigma Instruction Manual 1930" {
            Expect.equal
                ({
                    defaultEnigma with
                        Reflector = ReflectorA
                        Left = Rotor2
                        Middle = Rotor1
                        Right = Rotor3
                 }
                 |> withRotorPositions 'A' 'B' 'L'
                 |> withRingSettings 'X' 'M' 'V'
                 |> withPlugBoard "AM FI NV PS TU WZ"
                 |> Machine.translate
                     "GCDSE AHUGW TQGRK VLFGX UCALX VYMIG MMNMF DXTGN VHVRM MEVOU YFZSL RHDRR XFJWC FHUHM UNZEF RDISI KBGPM YVXUZ")
                "FEIND LIQEI NFANT ERIEK OLONN EBEOB AQTET XANFA NGSUE DAUSG ANGBA ERWAL DEXEN DEDRE IKMOS TWAER TSNEU STADT"
                "Does not match"
        }
        test "Operation Barbarossa Part 1" {
            Expect.equal
                ({
                    defaultEnigma with
                        Reflector = ReflectorB
                        Left = Rotor2
                        Middle = Rotor4
                        Right = Rotor5
                 }
                 |> withRotorPositions 'B' 'L' 'A'
                 |> withRingSettings 'B' 'U' 'L'
                 |> withPlugBoard "AV BS CG DL FU HZ IN KM OW RX"
                 |> Machine.translate
                     "EDPUD NRGYS ZRCXN UYTPO MRMBO FKTBZ REZKM LXLVE FGUEY SIOZV EQMIK UBPMM YLKLT TDEIS MDICA GYKUA CTCDO MOHWX MUUIA UBSTS LRNBZ SZWNR FXWFY SSXJZ VIJHI DISHP RKLKA YUPAD TXQSP INQMA TLPIF SVKDA SCTAC DPBOP VHJK")
                "AUFKL XABTE ILUNG XVONX KURTI NOWAX KURTI NOWAX NORDW ESTLX SEBEZ XSEBE ZXUAF FLIEG ERSTR ASZER IQTUN GXDUB ROWKI XDUBR OWKIX OPOTS CHKAX OPOTS CHKAX UMXEI NSAQT DREIN ULLXU HRANG ETRET ENXAN GRIFF XINFX RGTX"
                "Does not match"
        }
    ]

module PBT =
    type ValidTextGen() =
        static member private isValidChar = fun c -> Char.IsLetter c && Char.IsUpper c

        static member GenerateString() =
            Arb.Default.String()
            |> Arb.filter (function
                | null
                | "" -> false
                | text when text |> Seq.forall ValidTextGen.isValidChar -> true
                | _ -> false)

        static member GenerateChar() =
            Arb.Default.Char() |> Arb.filter ValidTextGen.isValidChar

    let testEnigma =
        {
            defaultEnigma with
                Reflector = ReflectorB
                Left = Rotor2
                Middle = Rotor4
                Right = Rotor5
        }
        |> withRotorPositions 'T' 'E' 'D'
        |> withRingSettings 'A' 'B' 'C'
        |> withPlugBoard "AB VS DG CL HU FZ KN IM RW OX"

    let fsCheckConfig = {
        FsCheckConfig.defaultConfig with
            arbitrary = [ typeof<ValidTextGen> ]
    }

    let testTranslate text = testEnigma |> Machine.translate text

let propertyBasedTests =
    testList "Property Based Tests" [
        testPropertyWithConfig PBT.fsCheckConfig "Encrypting and decrypting text always gives the same text"
        <| fun text ->
            Expect.equal
                (text |> PBT.testTranslate |> PBT.testTranslate)
                text
                "Encrypted text does not match original text."

        testPropertyWithConfig PBT.fsCheckConfig "Encrypted and decrypted text are never the same"
        <| fun text -> Expect.notEqual (PBT.testTranslate text) text "Encrypted text matches original text."

        testPropertyWithConfig PBT.fsCheckConfig "Encrypted and decrypted text are always the same length"
        <| fun text ->
            Expect.equal
                (PBT.testTranslate text).Length
                text.Length
                "Encrypted text length does not match original text length."

        testPropertyWithConfig
            PBT.fsCheckConfig
            "Encrypting the same character multiple times produces different results"
        <| fun letter ->
            String(Array.init 5 (fun _ -> letter))
            |> PBT.testTranslate
            |> Seq.distinct
            |> Seq.length > 1
    ]

[<Tests>]
let allTests = testList "All Tests" [ unitTests; enigmaTests; propertyBasedTests ]

[<EntryPoint>]
let main args = runTestsWithCLIArgs [] args allTests
