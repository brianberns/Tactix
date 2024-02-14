namespace Tactix

// https://www.ma.imperial.ac.uk/~buzzard/lean_together/source/contents.html

/// A type corresponds to a proposition that might be provable.
/// E.g. P -> Q.
type Type =
    | Primitive of name : string
    | Function of Type * Type

/// A term is an instance of a type and proves the corresponding
/// proposition. E.g. HP : P.
type Term =
    {
        /// Name of this term.
        Name : string

        /// Type of this term.
        Type : Type
    }

module Term =

    /// Creates a term of the given type.
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
