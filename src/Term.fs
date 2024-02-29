namespace Tactix

/// A term is evidence of a value and proves the corresponding
/// proposition. Can also be interpreted as an instance of a
/// type. E.g. HP : P.
type Term =
    {
        /// Value for which this term is evidence.
        Value : Value
    }

    /// Display string.
    override term.ToString() =
        $"H{term.Value}"

module Term =

    /// Creates a term.
    let create value =
        { Value = value }

    /// Matches a function term.
    let (|Function|_|) term =
        match term.Value with
            | Boolean (Implication (p, q)) -> Some (p, q)
            | _ -> None

    /// Matches a product term.
    let (|Product|_|) term =
        match term.Value with
            | Boolean (And bools) -> Some bools
            | _ -> None

    /// Matches a sum term.
    let (|Sum|_|) term =
        match term.Value with
            | Boolean (Or bools) -> Some bools
            | _ -> None

    /// Matches a not term.
    let (|Not|_|) term =
        match term.Value with
            | Boolean (Not bool) -> Some bool
            | _ -> None

    /// Matches an addition term.
    let (|Addition|_|) term =
        match term.Value with
            | NaturalNumber (Addition (a, b)) -> Some (a, b)
            | _ -> None
