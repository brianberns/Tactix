namespace Tactix

/// A type corresponds to a proposition that might be provable.
type Type =

    /// Atomic proposition, such as P.
    | Primitive of name : string

    /// Implication, such as P → Q.
    | Function of Type * Type

    /// Conjuction (and), such as P ∧ Q.
    | Product of List<Type>

    /// Disjuction (or), such as P ∨ Q.
    | Sum of List<Type>

    override typ.ToString() =

        let toString sep types =
            types
                |> Seq.map string
                |> String.concat (string sep)

        match typ with
            | Primitive name -> name
            | Function (P, Q) -> $"({P}→{Q})"
            | Product types -> $"({toString '∧' types})"
            | Sum types -> $"({toString '∨' types})"
