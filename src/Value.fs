namespace Tactix

type NaturalNumber =
    | Zero
    | Variable of name : string
    | Successor of NaturalNumber
    | Addition of NaturalNumber * NaturalNumber

    /// Display string.
    override nat.ToString() =
        match nat with
            | Zero -> "0"
            | Variable name -> name
            | Successor n -> $"S({n})"
            | Addition (a, b) -> $"({a} + {b})"

module NaturalNumber =

    /// Is the given number primitive?
    let isPrimitive = function
        | Zero
        | Variable _ -> true
        | _ -> false

/// A Boolean value. Can also be interpreted as a proposition.
type Boolean =

    | True
    | False

    /// Named variable value, such as P.
    | Variable of name : string

    /// Implication , such as P → Q.
    | Implication of Boolean * Boolean

    /// Conjunction, such as P ∧ Q.
    | And of List<Boolean>

    /// Disjuction, such as P ∨ Q.
    | Or of List<Boolean>

    /// Negation, such as ¬P.
    | Not of Boolean

    /// Display string.
    override bool.ToString() =

        let toString sep values =
            values
                |> Seq.map string
                |> String.concat (string sep)

        match bool with
            | True -> "true"
            | False -> "false"
            | Variable name -> name
            | Implication (p, q) -> $"({p}→{q})"
            | And values -> $"({toString '∧' values})"
            | Or values -> $"({toString '∨' values})"
            | Not p -> $"¬{p}"

module Boolean =

    /// Is the given Boolean primitive?
    let isPrimitive = function
        | Variable _ -> true
        | _ -> false

type Value =
    | NaturalNumber of NaturalNumber
    | Boolean of Boolean

    /// Display string.
    override value.ToString() =
        match value with
            | NaturalNumber nat -> string nat
            | Boolean bool -> string bool

module Value =

    /// Is the given value primitive?
    let isPrimitive = function
        | Boolean bool -> Boolean.isPrimitive bool
        | NaturalNumber nat -> NaturalNumber.isPrimitive nat
