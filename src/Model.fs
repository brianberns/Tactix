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

[<RequireQualifiedAccess>]
type ObjectType = Type | Term

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

    /// Expands type aliases.
    | ExpandAliases of ObjectType * ProofCaseKey

module Model =

    let init () =
        let settings =
            let settings = Settings.get ()
            let levelIdx =
                min
                    settings.LevelIndex
                    (Level.levels.Length - 1)
            { settings with LevelIndex = levelIdx }
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

    let private setHighlight highlight model =
        { model with Highlight = highlight }

    let private addTactic tactic caseKey model =

        let case = model.Proof.CaseMap[caseKey]
        let cases = ProofCase.add tactic case
        assert(not cases.IsEmpty)

        let proof =
            cases
                |> List.tryExactlyOne
                |> Option.map (fun case' ->
                    model.Proof
                        |> Proof.update caseKey case')
                |> Option.defaultWith (fun () ->
                    model.Proof
                        |> Proof.remove caseKey
                        |> Proof.addMany cases)
        { model with
            Proof = proof
            Highlight = Highlight.None }

    let private enableAudio enable model =
        let settings =
            { model.Settings with AudioEnabled = enable }
        Settings.save settings   // side-effect
        { model with Settings = settings }

    let private startLevel levelIdx model =

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

    let private expandAliases objType caseKey model =

        let rec loop = function
            | Primitive name -> Primitive name
            | Function (P, Q) -> Function (loop P, loop Q)
            | Product types -> List.map loop types |> Product
            | Sum types -> List.map loop types |> Sum
            | Alias (_, _, rhs) -> rhs   // one level only

        let case =
            let case = model.Proof.CaseMap[caseKey]
            match objType with
                | ObjectType.Type ->
                    { case with
                        GoalOpt = Option.map loop case.GoalOpt }
                | ObjectType.Term ->
                    { case with
                        Terms =
                            set [
                                for term in case.Terms do
                                    loop term.Type |> Term.create
                            ] }
        let caseMap = model.Proof.CaseMap.Add(caseKey, case)

        { model with
            Proof =
                { model.Proof with CaseMap = caseMap } }

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
                | ExpandAliases (objType, caseKey) ->
                    expandAliases objType caseKey model
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
