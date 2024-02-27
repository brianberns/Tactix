namespace Tactix

/// A term is an instance of a type and proves the corresponding
/// proposition. E.g. HP : P.
type Term =
    {
        /// Type of this term.
        Type : Type
    }

    /// Display string.
    override term.ToString() =
        $"H{term.Type}"

module Term =

    /// Creates a term of the given type.
    let create typ =
        { Type = typ }

    /// Matches a function term.
    let (|Function|_|) term =
        match term.Type with
            | Function (p, q) -> Some (p, q)
            | _ -> None

    /// Matches a product term.
    let (|Product|_|) term =
        match term.Type with
            | Product types -> Some types
            | _ -> None

    /// Matches a sum term.
    let (|Sum|_|) term =
        match term.Type with
            | Sum types -> Some types
            | _ -> None

    /// Matches a not term.
    let (|Not|_|) term =
        match term.Type with
            | Not typ -> Some typ
            | _ -> None
