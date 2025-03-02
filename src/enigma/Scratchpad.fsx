#load "Domain.fs" "Configuration.fs"
open Enigma

//Please decrypt me :)
let machine =
    {
        defaultEnigma with
            Left = Rotor3
            Middle = Rotor6
            Right = Rotor8
            Debug = true
    }
    |> withRotorOffsets 'U' 'Z' 'V'
    |> withRingSettings 'A' 'H' 'M'
    |> withPlugBoard "AN EZ HK IJ LR MQ OT PV SW UX"

machine
|> Machine.translate
    "YKAE NZAP MSCH ZBFO CUVM RMDP YCOF HADZ IZME FXTH FLOL PZLF GGBO TGOX GRET DWTJ IQHL MXVJ WKZU ASTR"

let encrypted =
    defaultEnigma
    |> withRotorOffsets 'U' 'Z' 'V'
    |> withRingSettings 'A' 'H' 'M'
    |> Machine.translate "High quality IT solutions that deliver value fast Powered by functional first technology"

#load "Bombe.fs"

open Enigma

let solution =
    Bombe.tryFindSolution
        "JCEG OJWTCKA KW NIVQIHPJB SZLS LVJQYHE YPNKA MSWX HAKKEBT CV EFFFFTZFIN VJPXX CTKZQUSEFX"
        "HIGH QUALITY IT"
        (fun (m, progress, rate, elapsed, remaining) ->
            printfn $"%.2f{progress}%% ({rate} sims/sec) {elapsed} elapsed, {remaining} remaining.")

let v, m = solution |> Option.get

m
|> Machine.translate "JCEG OJWTCKA KW NIVQIHPJB SZLS LVJQYHE YPNKA MSWX HAKKEBT CV EFFFFTZFIN VJPXX CTKZQUSEFX"
