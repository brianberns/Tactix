namespace Tactix

open Elmish

/// Current state of the game.
type Model =
    {
        /// 0-based index of the current level.
        LevelIndex : int

        /// Current state of the proof.
        Proof : Proof

        /// Currently highlighted term, type, or nothing.
        Highlighted : Choice<unit, Term, Type>

        /// Audio is currently enabled or disabled?
        AudioEnabled : bool
    }

/// Message to change the current state of the game.
type Message =

    /// Highlights the given term, type, or nothing.
    | Highlight of Choice<unit, Term, Type>

    /// Adds the given tactic to the proof.
    | AddTactic of Tactic

    /// Enables/disables audio.
    | EnableAudio of bool

    /// Sarts the given 0-based level.
    | StartLevel of int

module Model =

    let init () =
        let levelIdx = 0
        let level = Level.levels[levelIdx]
        let proof = Level.initializeProof level
        let model =
            {
                LevelIndex = levelIdx
                Proof = proof
                Highlighted = Choice1Of3 ()
                AudioEnabled = true
            }
        model, Cmd.none

    let private updateHighlight choice model =
        assert(model.Highlighted <> choice)
        { model with Highlighted = choice }

    let private updateAddTactic tactic model =
        { model with
            Proof = Proof.add tactic model.Proof }

    let private updateEnableAudio enable model =
        assert(model.AudioEnabled <> enable)
        { model with AudioEnabled = enable }

    let private updateStartLevel levelIdx model =
        let proof = Level.initializeProof Level.levels[levelIdx]
        { model with
            LevelIndex = levelIdx
            Proof = proof
            Highlighted = Choice1Of3 () }

    /// Updates the model based on the given message.
    let update msg model =
        let model' =
            match msg with
                | Highlight choice ->
                    updateHighlight choice model
                | AddTactic tactic ->
                    updateAddTactic tactic model
                | EnableAudio enable ->
                    updateEnableAudio enable model
                | StartLevel levelIdx ->
                    updateStartLevel levelIdx model
        let cmd =
            if model'.Proof.GoalOpt.IsNone then
                Cmd.OfAsync.perform
                    (fun () -> Async.Sleep 800)
                    ()
                    (fun () ->
                        if model.AudioEnabled then
                            Audio.playDiscovery ()   // to-do: move this side-effect into the view
                        StartLevel (model'.LevelIndex + 1))
            else
                Cmd.none
        model', cmd

module Message =

    let noHighlight = Highlight (Choice1Of3 ())

    let highlightTerm term = Highlight (Choice2Of3 term)

    let highlightType typ = Highlight (Choice3Of3 typ)
