namespace Tactix

/// A type corresponds to a proposition that might be provable.
type Type =

    /// Atomic proposition, such as P.
    | Primitive of name : string

    /// Implication, such as P → Q.
    | Function of Type * Type

    /// Conjunction (and), such as P ∧ Q.
    | Product of List<Type>

    /// Disjuction (or), such as P ∨ Q.
    | Sum of List<Type>

    /// Negation, such as ¬P.
    | Not of Type

    /// Equality.
    | Equal of NaturalNumber * NaturalNumber

    /// Display string.
    override typ.ToString() =

        let toString sep types =
            types
                |> Seq.map string
                |> String.concat (string sep)

        match typ with
            | Primitive name -> name
            | Function (p, q) -> $"({p}→{q})"
            | Product types -> $"({toString '∧' types})"
            | Sum types -> $"({toString '∨' types})"
            | Not p -> $"¬{p}"
            | Equal (a, b) -> $"{a}={b}"

module Type =

    /// Is the given type primitiv?
    let isPrimitive = function
        | Primitive _ -> true
        | _ -> false
