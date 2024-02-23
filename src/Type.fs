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

    /// Type alias, such as Not<T> = T → false.
    | Alias of name : string * parms : List<Type> * rhs : Type

    /// Display string.
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
            | Alias (name, parms, rhs) ->
                $"({name}<{toString parms}> = {rhs})"

module Type =

    /// Is the given type primitiv?
    let isPrimitive = function
        | Primitive _ -> true
        | _ -> false

    /// A type with a single, well-known value. AKA "true",
    /// "one", "unit", etc.
    let top = Primitive "top"

    /// A type with no values. AKA "false", "zero", "absurd",
    /// etc.
    let bottom = Primitive "bottom"

    /// Not<T> = T → false.
    let not typ =
        Alias ("Not", [typ], Function (typ, bottom))
