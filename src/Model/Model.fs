namespace Tactix

open Elmish

/// Highlighted object.
[<RequireQualifiedAccess>]
type Highlight =
    | None
    | Term of Term * ProofCaseKey
    | Goal of Type * ProofCaseKey

/// Current state of the game.
type Model =
    {
        /// Current settings.
        Settings : Settings

        /// Current state of the proof.
        Proof : Proof

        /// Currently highlighted object.
        Highlight : Highlight

        /// Current instructions, or empty string.
        Instructions : string
    }

    /// Is the given term highlighted?
    member model.IsHighlighted(term, caseKey) =
        model.Highlight = Highlight.Term (term, caseKey)

    /// Is the given goal highlighted?
    member model.IsHighlighted(goal, caseKey) =
        model.Highlight = Highlight.Goal (goal, caseKey)

/// Message to change the current state of the game.
type Message =

    /// Highlights the given term, type, or nothing.
    | Highlight of Highlight

    /// Adds the given tactic to the proof case.
    | AddTactic of Tactic * ProofCaseKey

    /// Enables/disables audio.
    | EnableAudio of bool

    /// Starts the given 0-based level.
    | StartLevel of int

    /// Clears current instructions.
    | ClearInstructions

module Model =

    /// Creates a model from the given settings.
    let private create settings =

            // ensure valid level
        let levelIdx =
            max
                (min
                    settings.LevelIndex
                    (Level.levels.Length - 1))
                0
        let settings' =
            { settings with LevelIndex = levelIdx }
        let level = Level.levels[levelIdx]

        {
            Settings = settings'
            Proof = Level.initializeProof level
            Highlight = Highlight.None
            Instructions = level.Instructions
        }

    /// Initializes a model at the user's current level.
    let init () =
        create (Settings.get()), Cmd.none

    /// Sets the current highlighted object.
    let private setHighlight highlight model =
        { model with Highlight = highlight }

    /// Adds a tactic to the model's proof.
    let private addTactic tactic caseKey model =
        let proof =
            let cases =
                model.Proof.CaseMap[caseKey]
                    |> ProofCase.add tactic
            model.Proof
                |> Proof.remove caseKey
                |> Proof.addMany (Some caseKey) cases
        { model with
            Proof = proof
            Highlight = Highlight.None }

    /// Enables/disables audio.
    let private enableAudio enable model =
        let settings =
            { model.Settings with AudioEnabled = enable }
        Settings.save settings   // side-effect
        { model with Settings = settings }

    /// Starts a level.
    let private startLevel levelIdx model =
        let model =
            create {
                model.Settings with
                    LevelIndex = levelIdx }
        Settings.save model.Settings   // side-effect
        model

    /// Clears current instructions.
    let private clearInstructions (model : Model) =
        { model with Instructions = "" }

    /// Updates the model based on the given message.
    let update msg model =
        let model' =
            match msg with
                | Highlight highlight ->
                    setHighlight highlight model
                | AddTactic (tactic, caseKey) ->
                    addTactic tactic caseKey model
                | EnableAudio enable ->
                    enableAudio enable model
                | StartLevel levelIdx ->
                    startLevel levelIdx model
                | ClearInstructions ->
                    clearInstructions model
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

    /// Clears highlighting.
    let noHighlight = Highlight Highlight.None

    /// Highlights the given term.
    let highlightTerm term caseKey =
        Highlight (Highlight.Term (term, caseKey))

    /// Highlights the given goal.
    let highlightGoal goal caseKey =
        Highlight (Highlight.Goal (goal, caseKey))
