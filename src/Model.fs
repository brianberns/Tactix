namespace Tactix

open Elmish

type Model =
    {
        Proof : Proof
        HighlightedTermIds : Set<int>
    }

type Msg =
    | HighlightTerm of (*id*) int * bool

module Model =

    let init () =
        let model =
            {
                Proof =
                    {
                        Goal = P
                        Terms = [
                            Term.create P
                            Term.create Q
                            Term.create R
                        ]
                        Tactics = [ Exact; Intro; Apply ]
                    }
                HighlightedTermIds = Set.empty
            }
        model, Cmd.none

    let update (msg : Msg) (model : Model) =
        let model' =
            match msg with
                | HighlightTerm (termId, highlight) ->
                    let termIds =
                        assert(model.HighlightedTermIds.Contains(termId) = not highlight)
                        if highlight then
                            model.HighlightedTermIds.Add(termId)
                        else
                            model.HighlightedTermIds.Remove(termId)
                    { model with HighlightedTermIds = termIds }
        model', Cmd.none
