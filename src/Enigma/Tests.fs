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
                 |> withRotorOffsets 'A' 'B' 'C'
                 |> Machine.translate "AEFAEJXXBNXYJTY")
                "CONGRATULATIONS"
                "Does not match"
        }
        test "Should translate a message with rotor turnover" {
            Expect.equal
                (defaultEnigma |> withRotorOffsets 'A' 'B' 'R' |> Machine.translate "MABE KGZXSG")
                "TURN MIDDLE"
                "Does not match"
        }
        test "Should translate a message with double stepping" {
            Expect.equal
                (defaultEnigma |> withRotorOffsets 'A' 'D' 'S' |> Machine.translate "RZFOG FYHPL")
                "TURNS THREE"
                "Does not match"
        }
        test "Should translate a message with ring settings" {
            Expect.equal
                (defaultEnigma
                 |> withRotorOffsets 'X' 'Y' 'Z'
                 |> withRingSettings 'J' 'N' 'U'
                 |> Machine.translate "QKTP EBZIUK")
                "GOOD RESULT"
                "Does not match"
        }
        test "Should translate a message with custom rotors" {
            Expect.equal
                (defaultEnigma
                 |> withRotors Rotor2 Rotor3 Rotor1
                 |> withRotorOffsets 'X' 'Y' 'Z'
                 |> withRingSettings 'J' 'N' 'U'
                 |> Machine.translate "WMUOMJ YRLFLA")
                "CUSTOM ROTORS"
                "Does not match"
        }
        test "Should translate a message with a plugboard" {
            Expect.equal
                (defaultEnigma
                 |> withRotorOffsets 'V' 'Q' 'Q'
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
                 |> withRotorOffsets 'A' 'B' 'L'
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
                 |> withRotorOffsets 'B' 'L' 'A'
                 |> withRingSettings 'B' 'U' 'L'
                 |> withPlugBoard "AV BS CG DL FU HZ IN KM OW RX"
                 |> Machine.translate
                     "EDPUD NRGYS ZRCXN UYTPO MRMBO FKTBZ REZKM LXLVE FGUEY SIOZV EQMIK UBPMM YLKLT TDEIS MDICA GYKUA CTCDO MOHWX MUUIA UBSTS LRNBZ SZWNR FXWFY SSXJZ VIJHI DISHP RKLKA YUPAD TXQSP INQMA TLPIF SVKDA SCTAC DPBOP VHJK")
                "AUFKL XABTE ILUNG XVONX KURTI NOWAX KURTI NOWAX NORDW ESTLX SEBEZ XSEBE ZXUAF FLIEG ERSTR ASZER IQTUN GXDUB ROWKI XDUBR OWKIX OPOTS CHKAX OPOTS CHKAX UMXEI NSAQT DREIN ULLXU HRANG ETRET ENXAN GRIFF XINFX RGTX"
                "Does not match"
        }
        test "Operation Barbarossa Part 2" {
            Expect.equal
                ({
                    defaultEnigma with
                        Reflector = ReflectorB
                        Left = Rotor2
                        Middle = Rotor4
                        Right = Rotor5
                 }
                 |> withRotorOffsets 'L' 'S' 'D'
                 |> withRingSettings 'B' 'U' 'L'
                 |> withPlugBoard "AV BS CG DL FU HZ IN KM OW RX"
                 |> Machine.translate
                     "SFBWD NJUSE GQOBH KRTAR EEZMW KPPRB XOHDR OEQGB BGTQV PGVKB VVGBI MHUSZ YDAJQ IROAX SSSNR EHYGG RPISE ZBOVM QIEMM ZCYSG QDGRE RVBIL EKXYQ IRGIR QNRDN VRXCY YTNJR")
                "DREIG EHTLA NGSAM ABERS IQERV ORWAE RTSXE INSSI EBENN ULLSE QSXUH RXROE MXEIN SXINF RGTXD REIXA UFFLI EGERS TRASZ EMITA NFANG XEINS SEQSX KMXKM XOSTW XKAME NECXK"
                "Does not match"
        }
        test "Scharnhorst, 1943" {
            Expect.equal
                ({
                    defaultEnigma with
                        Left = Rotor3
                        Middle = Rotor6
                        Right = Rotor8
                 }
                 |> withRotorOffsets 'U' 'Z' 'V'
                 |> withRingSettings 'A' 'H' 'M'
                 |> withPlugBoard "AN EZ HK IJ LR MQ OT PV SW UX"
                 |> Machine.translate
                     "YKAE NZAP MSCH ZBFO CUVM RMDP YCOF HADZ IZME FXTH FLOL PZLF GGBO TGOX GRET DWTJ IQHL MXVJ WKZU ASTR")
                "STEU EREJ TANA FJOR DZAN STAN DORT QUAA ACCC VIER NEUS NEUN ZWOF AHRT ZWON ULSM XXSC HZRN HORS THCO"
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
        |> withRotorOffsets 'T' 'E' 'D'
        |> withRingSettings 'A' 'B' 'C'
        |> withPlugBoard "AB VS DG CL HU FZ KN IM RW OX"

    let fsCheckConfig = {
        FsCheckConfig.defaultConfig with
            arbitrary = [ typeof<ValidTextGen> ]
    }

    let testTranslate text = testEnigma |> Machine.translate text

let propertyBasedTests =
    testList "Property Based Tests" [
        testPropertyWithConfig
            PBT.fsCheckConfig
            "Encrypting and then decrypting text always gives back the original text"
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
            "Encrypting the same character multiple times always produces different results"
        <| fun letter ->
            String(Array.init 5 (fun _ -> letter))
            |> PBT.testTranslate
            |> Seq.distinct
            |> Seq.length > 1
    ]

[<Tests>]
let allTests = testList "All Tests" [ unitTests; enigmaTests; propertyBasedTests ]

[<EntryPoint>]
let main args =
    // let solution =
    //     Bombe.tryFindSolution
    //         "EDPUD NRGYS ZRCXN UYTPO MRMBO FKTBZ REZKM LXLVE FGUEY SIOZV EQMIK UBPMM YLKLT TDEIS MDICA GYKUA CTCDO MOHWX MUUIA UBSTS LRNBZ SZWNR FXWFY SSXJZ VIJHI DISHP RKLKA YUPAD TXQSP INQMA TLPIF SVKDA SCTAC DPBOP VHJK"
    //         "AUFKL"
    //         (fun (m, progress, rate, elapsed, remaining) ->
    //             printfn $"%.2f{progress}%% ({rate} sims/sec) {elapsed} elapsed, {remaining} remaining.")

    // 0
    runTestsWithCLIArgs [] args allTests
