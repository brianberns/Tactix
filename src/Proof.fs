namespace Tactix

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
    }

module Term =

    let mutable private curId = 0

    let create typ =
        curId <- curId + 1
        {
            Id = curId
            Type = typ
        }

type Tactic =
    | Exact
    | Intro
    | Apply

type Proof =
    {
        Goal : Type
        Terms : List<Term>
        Tactics : List<Tactic>
    }
