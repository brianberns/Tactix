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
    | Exact
    | Intro
    | Apply

type Proof =
    {
        Goal : Type
        Terms : List<Term>
        Tactics : List<Tactic>
    }
