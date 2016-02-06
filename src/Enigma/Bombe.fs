module Enigma.Bombe

open Nessos.Streams
open System.Diagnostics
open System
open System.Timers

module private SequenceGeneration =
    let private endSequence = Array.create 3 'Z'
    let private shiftLetter index (letters:char array) =
        if letters.[index] = 'Z' then
            letters.[index] <- 'A'
            true
        else
            letters.[index] <- (int letters.[index] + 1) |> char
            false
    
    let rec generateSequence (letters:char array) =
        seq {
            yield letters
            if letters <> endSequence then
                ((false, 0), letters)
                ||> Array.fold(fun (shifted, index) _ ->
                    let wasShifted =
                        if shifted || index = 0 then
                            let wasShifted = letters |> shiftLetter index
                            wasShifted
                        else false
                    wasShifted, index + 1)
                |> ignore
                yield! generateSequence letters
        }

    let rotors =
        let rotorComponents = [ Components.Rotor1; Components.Rotor2; Components.Rotor3; Components.Rotor4; Components.Rotor5 ]
        seq {
            for a in 0 .. 4 do
                for b in 0 .. 4 do
                    for c in 0 .. 4 do
                        if (a <> b && a <> c && b <> c) then
                            yield rotorComponents.[a], rotorComponents.[b], rotorComponents.[c]
        } |> Seq.toArray
    
type private Status = Update of Enigma | Complete
let tryFindSolution encryptedText crib logger =
    let permutations =
        SequenceGeneration.generateSequence (Array.create 5 'A')
        |> ParStream.ofSeq

    let recorder = MailboxProcessor.Start(fun inbox ->
        let mutable attempts = 0
        let mutable lastEnigma = defaultEnigma
        let timer = new Timer((TimeSpan.FromSeconds 15.).TotalMilliseconds)
        let start = Stopwatch.StartNew()
        let totalPermutations = 26. ** 5.
        let perc = 100. / totalPermutations

        let subscription =
            timer.Elapsed.Subscribe(fun _ ->
                let elapsed = start.Elapsed
                let rate = float attempts / elapsed.TotalSeconds
                let estimated = (totalPermutations / rate) - elapsed.TotalSeconds
                logger (lastEnigma, float attempts * perc, int rate, elapsed, TimeSpan.FromSeconds estimated))
        timer.Start()

        async {
            while true do
                let! message = inbox.Receive()
                match message with
                | Update enigma ->
                    lastEnigma <- enigma
                    attempts <- attempts + 1
                | Complete -> subscription.Dispose()
        })
    permutations
    |> ParStream.mapi(fun index ([| a;b;c;d;e |]) ->
        index,
        defaultEnigma
        |> withWheelPositions a b c
        |> withRingSettings 'A' d e) // first ring setting does nothing
    |> ParStream.tryFind(fun (_, machine) ->
        recorder.Post (Update machine)
        let translation = machine |> translate encryptedText
        let matched = translation.StartsWith crib
        if matched then recorder.Post Complete
        matched)