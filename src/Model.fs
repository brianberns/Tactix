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
        Id : int
        Type : Type
        Highlight : bool
    }

module Term =

    let mutable private nextId = 0

    let create typ =
        let id = nextId
        nextId <- nextId + 1
        {
            Id = id
            Type = typ
            Highlight = false
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

type Msg =
    | HighlightTerm of (*id*) int * bool

module Model =

    let init () =
        let model =
            {
                Goal = P
                Terms = [
                    Term.create P
                    Term.create Q
                    Term.create R
                ]
                Tactics = [ Exact; Intro; Apply ]
            }
        model, Cmd.none

    let update (msg : Msg) (model : Model) =
        let model' =
            match msg with
                | HighlightTerm (termId, highlight) ->
                    {
                        model with
                            Terms =
                                model.Terms
                                    |> List.map (fun term ->
                                        if term.Id = termId then
                                            assert(term.Highlight <> highlight)
                                            { term with Highlight = highlight }
                                        else term)
                    }
        model', Cmd.none
