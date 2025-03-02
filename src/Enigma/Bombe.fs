module Enigma.Bombe

open System.Diagnostics
open System
open System.Timers

module private SequenceGeneration =
    let private endSequence = Array.create 3 'Z'

    let private shiftLetter index (letters: char array) =
        if letters[index] = 'Z' then
            letters[index] <- 'A'
            true
        else
            letters[index] <- (int letters[index] + 1) |> char
            false

    let rec generateSequence (letters: char array) = seq {
        letters

        if letters <> endSequence then
            ((false, 0), letters)
            ||> Array.fold (fun (shifted, index) _ ->
                let wasShifted =
                    if shifted || index = 0 then
                        let wasShifted = letters |> shiftLetter index
                        wasShifted
                    else
                        false

                wasShifted, index + 1)
            |> ignore

            yield! generateSequence letters
    }

    let rotors =
        let rotorComponents = [ Rotor1; Rotor2; Rotor3; Rotor4; Rotor5 ]

        [|
            for a in 0..4 do
                for b in 0..4 do
                    for c in 0..4 do
                        if a <> b && a <> c && b <> c then
                            rotorComponents[a], rotorComponents[b], rotorComponents[c]
        |]

type private Status =
    | Update of Machine
    | Complete

let tryFindSolution encryptedText (crib: string) logger =
    let permutations = SequenceGeneration.generateSequence (Array.create 5 'A')

    let recorder =
        MailboxProcessor.Start(fun inbox ->
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
                    let estimated = totalPermutations / rate - elapsed.TotalSeconds
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
    |> Seq.indexed
    |> Seq.choose (fun (index, permutations) ->
        match permutations with
        | [| a; b; c; d; e |] -> Some(index, defaultEnigma |> withRotorOffsets a b c |> withRingSettings 'A' d e) // first ring setting does nothing
        | _ -> None)
    |> Seq.tryFind (fun (_, machine) ->
        recorder.Post(Update machine)
        let translation = machine |> Machine.translate encryptedText
        let matched = translation.StartsWith crib

        if matched then
            recorder.Post Complete

        matched)
