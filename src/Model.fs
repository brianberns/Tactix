namespace Tactix

open Elmish

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
