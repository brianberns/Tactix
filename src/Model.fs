namespace Tactix

open Elmish

// https://www.ma.imperial.ac.uk/~buzzard/lean_together/source/contents.html

/// A type corresponds to a proposition that might be provable.
/// E.g. 1 + 1 = 2.
type Type = P | Q | R

/// A term is an instance of a type and proves the corresponding
/// proposition.
type Term =
    {
        Type : Type
    }

type Tactic =
    | Exact
    | Intro
    | Apply

type Model =
    {
        Goal : Type
        Terms : List<Term>
        Tactics : List<Tactic>
    }

type Msg = unit

module Model =

    let init () =
        let model =
            {
                Goal = P
                Terms = [ { Type = P } ]
                Tactics = [ Exact; Intro; Apply ]
            }
        model, Cmd.none

    let update (msg : Msg) (model : Model) =
        model, Cmd.none
