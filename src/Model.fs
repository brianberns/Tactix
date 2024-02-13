namespace Tactix

open Elmish

[<RequireQualifiedAccess>]
type TacticType =
    | Exact
    | Intro
    | Apply

module TacticType =

    let emoji = function
        | TacticType.Exact -> "🎆"
        | TacticType.Intro -> "🚀"
        | TacticType.Apply -> "👣"

type Level =
    {
        Goal : Type
        Terms : Set<Term>
        TacticTypes : Set<TacticType>
        Instructions : string
    }

module Level =

    module private Term =

        let create typ =
            Term.create $"H{typ}" typ

    let levels =
        [|
            {
                Goal = P
                Terms =
                    set [
                        Term.create P
                        Term.create Q
                    ]
                TacticTypes =
                    set [
                        TacticType.Exact
                    ]
                Instructions =
                    let exact = TacticType.emoji TacticType.Exact
                    $"Drag {exact} onto the symbol that matches the goal."
            }
        |]

    let initializeProof level =
        {
            Goal = Some level.Goal
            Terms = level.Terms
        }

type Model =
    {
        LevelIndex : int
        Proof : Proof
        HighlightedTermNames : Set<string>
        AudioEnabled : bool
    }

type Msg =
    | HighlightTerm of (*name*) string * bool
    | AddTactic of Tactic
    | EnableAudio of bool

module Model =

    let init () =
        let levelIdx = 0
        let level = Level.levels[levelIdx]
        let model =
            {
                LevelIndex = levelIdx
                Proof = Level.initializeProof level
                HighlightedTermNames = Set.empty
                AudioEnabled = true
            }
        model, Cmd.none

    let private updateHighlightTerm termName highlight model =
        let termNames =
            assert(
                model.HighlightedTermNames.Contains(termName)
                    = not highlight)
            if highlight then
                model.HighlightedTermNames.Add(termName)
            else
                model.HighlightedTermNames.Remove(termName)
        { model with HighlightedTermNames = termNames }

    let private updateAddTactic tactic model =
        { model with Proof = Proof.add tactic model.Proof }

    let private updateEnableAudio enable model =
        assert(model.AudioEnabled <> enable)
        { model with AudioEnabled = enable }

    let update (msg : Msg) (model : Model) =
        let model' =
            match msg with
                | HighlightTerm (termName, highlight) ->
                    updateHighlightTerm termName highlight model
                | AddTactic tactic ->
                    updateAddTactic tactic model
                | EnableAudio enable ->
                    updateEnableAudio enable model
        model', Cmd.none
