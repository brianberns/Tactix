namespace Tactix

open Elmish

// https://www.ma.imperial.ac.uk/~buzzard/lean_together/source/contents.html

type Proposition = P | Q | R

type Hypothesis =
    {
        Proposition : Proposition
    }

type Tactic =
    | Exact
    | Intro
    | Apply

type Model =
    {
        Goal : Proposition
        Hypotheses : List<Hypothesis>
        Tactics : List<Tactic>
    }

type Msg = unit

module Model =

    let init () =
        let model =
            {
                Goal = P
                Hypotheses = [ { Proposition = P } ]
                Tactics = [ Exact; Intro; Apply ]
            }
        model, Cmd.none

    let update (msg : Msg) (model : Model) =
        model, Cmd.none
