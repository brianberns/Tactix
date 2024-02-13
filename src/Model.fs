namespace Tactix

open Elmish

[<RequireQualifiedAccess>]
type TacticType =
    | Exact
    | Intro
    | Apply

type Model =
    {
        Proof : Proof
        TacticTypes : List<TacticType>
        HighlightedTermNames : Set<string>
    }

type Msg =
    | HighlightTerm of (*name*) string * bool
    | AddTactic of Tactic

module Model =

    module private Term =

        let create typ =
            Term.create $"H{typ}" typ

    let init () =
        let model =
            {
                Proof =
                    {
                        Goal = Some P
                        Terms =
                            set [
                                Term.create P
                                Term.create Q
                                Term.create R
                            ]
                    }
                TacticTypes =
                    [
                        TacticType.Exact
                        TacticType.Intro
                        TacticType.Apply
                    ]
                HighlightedTermNames = Set.empty
            }
        model, Cmd.none

    let update (msg : Msg) (model : Model) =
        let model' =
            match msg with
                | HighlightTerm (termName, highlight) ->
                    let termNames =
                        assert(
                            model.HighlightedTermNames.Contains(termName)
                                = not highlight)
                        if highlight then
                            model.HighlightedTermNames.Add(termName)
                        else
                            model.HighlightedTermNames.Remove(termName)
                    { model with HighlightedTermNames = termNames }
                | AddTactic tactic ->
                    { model with Proof = Proof.add tactic model.Proof }
        model', Cmd.none
