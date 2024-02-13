namespace Tactix

open Elmish

type Model =
    {
        Proof : Proof
        HighlightedTermNames : Set<string>
    }

type Msg =
    | HighlightTerm of (*name*) string * bool

module Model =

    module private Term =

        let create typ =
            Term.create $"H{typ}" typ

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
                HighlightedTermNames = Set.empty
            }
        model, Cmd.none

    let update (msg : Msg) (model : Model) =
        let model' =
            match msg with
                | HighlightTerm (termName, highlight) ->
                    let termNames =
                        assert(model.HighlightedTermNames.Contains(termName) = not highlight)
                        if highlight then
                            model.HighlightedTermNames.Add(termName)
                        else
                            model.HighlightedTermNames.Remove(termName)
                    { model with HighlightedTermNames = termNames }
        model', Cmd.none
