namespace Tactix

open Elmish

[<RequireQualifiedAccess>]
type Highlight =
    | None
    | Term of Term * ProofCaseKey
    | Type of Type * ProofCaseKey

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

    member model.IsHighlighted(term, caseKey) =
        model.Highlight = Highlight.Term (term, caseKey)

    member model.IsHighlighted(typ, caseKey) =
        model.Highlight = Highlight.Type (typ, caseKey)

/// Message to change the current state of the game.
type Message =

    /// Highlights the given term, type, or nothing.
    | Highlight of Highlight

    /// Adds the given tactic to the proof case.
    | AddTactic of Tactic * ProofCaseKey

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

    let private updateAddTactic tactic caseKey model =
        let proof =
            let case = model.Proof.CaseMap[caseKey]
            let case' =
                case
                    |> ProofCase.tryAdd tactic
                    |> Option.defaultValue case
            Proof.update case' model.Proof
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
                | Highlight highlight ->
                    updateHighlight highlight model
                | AddTactic (tactic, case) ->
                    updateAddTactic tactic case model
                | EnableAudio enable ->
                    updateEnableAudio enable model
                | StartLevel levelIdx ->
                    updateStartLevel levelIdx model
        let cmd =
            if Proof.isComplete model'.Proof then
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

    let highlightTerm term caseKey =
        Highlight (Highlight.Term (term, caseKey))

    let highlightType typ caseKey =
        Highlight (Highlight.Type (typ, caseKey))
