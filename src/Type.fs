namespace Tactix

type NaturalNumber =
    | Zero
    | Successor of NaturalNumber

    /// Display string.
    override nat.ToString() =
        match nat with
            | Zero -> "0"
            | Successor n -> $"S({n})"

module NaturalNumber =

    /// Is the given number primitive?
    let isPrimitive num =
        num = Zero

/// A type corresponds to a proposition that might be provable.
type Proposition =

    /// Atomic proposition, such as P.
    | Primitive of name : string

    /// Implication, such as P → Q.
    | Impliction of Proposition * Proposition

    /// Conjunction (and), such as P ∧ Q.
    | Conjunction of List<Proposition>

    /// Disjuction (or), such as P ∨ Q.
    | Disjunction of List<Proposition>

    /// Negation, such as ¬P.
    | Not of Proposition

    /// Display string.
    override prop.ToString() =

        let toString sep types =
            types
                |> Seq.map string
                |> String.concat (string sep)

        match prop with
            | Primitive name -> name
            | Impliction (P, Q) -> $"({P}→{Q})"
            | Conjunction types -> $"({toString '∧' types})"
            | Disjunction types -> $"({toString '∨' types})"
            | Not P -> $"¬{P}"

module Proposition =

    /// Is the given proposition primitive?
    let isPrimitive = function
        | Primitive _ -> true
        | _ -> false

type Type =
    | NaturalNumber of NaturalNumber
    | Proposition of Proposition

module Type =

    /// Is the given type primitive?
    let isPrimitive = function
        | Proposition prop -> Proposition.isPrimitive prop
        | NaturalNumber num -> NaturalNumber.isPrimitive num
