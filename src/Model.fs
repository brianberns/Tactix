namespace Tactix

open Elmish

[<RequireQualifiedAccess>]
type Highlight =
    | None
    | Term of Term
    | Type of Type

/// Current state of the game.
type Model =
    {
        /// Current settings.
        Settings : Settings

        /// Current state of the proof.
        Proof : Proof

        /// Currently highlighted object.
        Highlight : Highlight
    }

    member model.IsHighlighted(term) =
        model.Highlight = Highlight.Term term

    member model.IsHighlighted(typ) =
        model.Highlight = Highlight.Type typ

/// Message to change the current state of the game.
type Message =

    /// Highlights the given term, type, or nothing.
    | Highlight of Highlight

    /// Adds the given tactic to the proof.
    | AddTactic of Tactic

    /// Enables/disables audio.
    | EnableAudio of bool

    /// Sarts the given 0-based level.
    | StartLevel of int

module Model =

    let init () =
        let settings = Settings.get ()
        let proof =
            Level.levels[settings.LevelIndex]
                |> Level.initializeProof
        let model =
            {
                Settings = settings
                Proof = proof
                Highlight = Highlight.None
            }
        model, Cmd.none

    let private updateHighlight highlight model =
        { model with Highlight = highlight }

    let private updateAddTactic tactic model =
        let proof =
            model.Proof
                |> Proof.tryAdd tactic
                |> Option.defaultValue model.Proof
        { model with
            Proof = proof
            Highlight = Highlight.None }

    let private updateEnableAudio enable model =
        let settings =
            { model.Settings with AudioEnabled = enable }
        Settings.save settings   // side-effect
        { model with Settings = settings }

    let private updateStartLevel levelIdx model =

        let levelIdx = levelIdx % Level.levels.Length
        let settings =
            { model.Settings with LevelIndex = levelIdx }
        Settings.save settings   // side-effect

        let proof = Level.initializeProof Level.levels[levelIdx]

        {
            Settings = settings
            Proof = proof
            Highlight = Highlight.None
        }

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
                        if model.Settings.AudioEnabled then
                            Audio.playDiscovery ()   // side-effect
                        StartLevel (model'.Settings.LevelIndex + 1))
            else
                Cmd.none
        model', cmd

module Message =

    let noHighlight = Highlight Highlight.None

    let highlightTerm term = Highlight (Highlight.Term term)

    let highlightType typ = Highlight (Highlight.Type typ)
