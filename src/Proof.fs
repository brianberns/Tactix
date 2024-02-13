namespace Tactix

// https://www.ma.imperial.ac.uk/~buzzard/lean_together/source/contents.html

/// A type corresponds to a proposition that might be provable.
/// E.g. 1 + 1 = 2.
type Type = P | Q | R

/// A term is an instance of a type and proves the corresponding
/// proposition.
type Term =
    {
        Name : string
        Type : Type
    }

module Term =

    let create name typ =
        {
            Name = name
            Type = typ
        }

type Tactic =
    | Exact of Term
    | Intro
    | Apply

type Proof =
    {
        Goal : Option<Type>
        Terms : Set<Term>
    }

module Proof =

    let add tactic proof =
        match tactic with
            | Exact term when
                proof.Terms.Contains(term)
                    && Some term.Type = proof.Goal ->
                {
                    Goal = None
                    Terms = proof.Terms.Remove(term)
                }
            | _ -> failwith "Unexpected"
