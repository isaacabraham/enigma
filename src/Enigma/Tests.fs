module Enigma.Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open System

[<Fact>]
let ``Should translate a message that only needs the right rotor to advance``() =
    (defaultEnigma |> withWheelPositions 'A' 'B' 'C')
    |> translate "AEFAEJXXBNXYJTY"
    =? "CONGRATULATIONS"
    
[<Fact>]
let ``Should translate a message with rotor turnover``() =
     (defaultEnigma |> withWheelPositions 'A' 'B' 'R')
    |> translate "MABE KGZXSG"
    =? "TURN MIDDLE"

[<Fact>]
let ``Should translate a message with double stepping``() =
    (defaultEnigma |> withWheelPositions 'A' 'D' 'S')
    |> translate "RZFOG FYHPL"
    =? "TURNS THREE"

[<Fact>]
let ``Should translate a message with ring settings``() =
    (defaultEnigma |> withWheelPositions 'X' 'Y' 'Z'
                   |> withRingSettings 'J' 'N' 'U')
    |> translate "QKTP EBZIUK"
    =? "GOOD RESULT"

[<Fact>]
let ``Should translate a message with a plugboard``() =
    (defaultEnigma |> withWheelPositions 'V' 'Q' 'Q'
                   |> withRingSettings 'J' 'N' 'U'
                   |> withPlugBoard "AP BR CM FZ GJ IL NT OV QS WX")
    |> translate "HABHV HL YDFN ADZY"
    =? "THATS IT WELL DONE"

[<Fact>]
let ``Enigma Instruction Manual 1930``() =
    ({ defaultEnigma
        with Reflector = Components.ReflectorA
             Left = Components.Rotor2
             Middle = Components.Rotor1
             Right = Components.Rotor3 }
        |> withRingSettings 'X' 'M' 'V'
        |> withWheelPositions 'A' 'B' 'L'
        |> withPlugBoard "AM FI NV PS TU WZ")
    |> translate "GCDSE AHUGW TQGRK VLFGX UCALX VYMIG MMNMF DXTGN VHVRM MEVOU YFZSL RHDRR XFJWC FHUHM UNZEF RDISI KBGPM YVXUZ"
    =? "FEIND LIQEI NFANT ERIEK OLONN EBEOB AQTET XANFA NGSUE DAUSG ANGBA ERWAL DEXEN DEDRE IKMOS TWAER TSNEU STADT"

[<Fact>]
let ``Operation Barbarossa Part 1``() =
    ({ defaultEnigma
          with Reflector = Components.ReflectorB
               Left = Components.Rotor2
               Middle = Components.Rotor4
               Right = Components.Rotor5 }
      |> withRingSettings 'B' 'U' 'L'
      |> withWheelPositions 'B' 'L' 'A'
      |> withPlugBoard "AV BS CG DL FU HZ IN KM OW RX")
    |> translate "EDPUD NRGYS ZRCXN UYTPO MRMBO FKTBZ REZKM LXLVE FGUEY SIOZV EQMIK UBPMM YLKLT TDEIS MDICA GYKUA CTCDO MOHWX MUUIA UBSTS LRNBZ SZWNR FXWFY SSXJZ VIJHI DISHP RKLKA YUPAD TXQSP INQMA TLPIF SVKDA SCTAC DPBOP VHJK"
    =? "AUFKL XABTE ILUNG XVONX KURTI NOWAX KURTI NOWAX NORDW ESTLX SEBEZ XSEBE ZXUAF FLIEG ERSTR ASZER IQTUN GXDUB ROWKI XDUBR OWKIX OPOTS CHKAX OPOTS CHKAX UMXEI NSAQT DREIN ULLXU HRANG ETRET ENXAN GRIFF XINFX RGTX"

let testEnigma =
    { defaultEnigma
         with Reflector = Components.ReflectorB
              Left = Components.Rotor2
              Middle = Components.Rotor4
              Right = Components.Rotor5 }
    |> withRingSettings 'A' 'B' 'C'
    |> withWheelPositions 'T' 'E' 'D'
    |> withPlugBoard "AB VS DG CL HU FZ KN IM RW OX"

//TODO: Create a proper FsCheck generator to only generate encryptable strings.
let (|Encryptable|Invalid|) =
    function
    | null
    | "" -> Invalid
    | text when text |> (not << Seq.exists Char.IsLetter) -> Invalid
    | text -> Encryptable text

[<Property(Verbose = true)>]
let ``Encrypting and decrypting text always gives the same text``(text) =
    match text with
    | Invalid -> true
    | Encryptable text ->
        let encrypted = testEnigma |> translate text
        let decrypted = testEnigma |> translate encrypted
        text.ToUpper() = decrypted

[<Property(Verbose = true)>]
let ``Encrypted and decrypted text are never the same``(text) =
    match text with
    | Invalid -> true
    | Encryptable text ->
        let encrypted = testEnigma |> translate text
        text.ToUpper() <> encrypted

[<Property(Verbose = true)>]
let ``Encrypted and decrypted text are always the same length``(text) =
    match text with
    | Invalid -> true
    | Encryptable text ->
        let encrypted = testEnigma |> translate text
        text.Length = encrypted.Length