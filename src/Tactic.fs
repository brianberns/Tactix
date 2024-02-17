namespace Tactix

// https://www.ma.imperial.ac.uk/~buzzard/lean_together/source/contents.html

/// Tactic type.
[<RequireQualifiedAccess>]
type TacticType =
    | Exact
    | Intro
    | Apply
    | Cases
    | Left
    | Right

/// A tactic used in a proof.
type Tactic =

    /// Term (HP : P) eliminates goal P.
    | Exact of Term

    /// Introduces term (HP : P) when goal is P -> Q, changing
    /// goal to just Q.
    | Intro of Term

    /// Applies term (HPQ : P -> Q) when goal is Q, changing
    /// goal to just P.
    | Apply of Term

    /// Breaks up an hypothesis (HP : P) into its component
    /// parts, creating goals for each constructor of P.
    | Cases of Term

    | Left
    | Right

    /// Tactic type.
    member tactic.Type =
        match tactic with
            | Exact _ -> TacticType.Exact
            | Intro _ -> TacticType.Intro
            | Apply _ -> TacticType.Apply
            | Cases _ -> TacticType.Cases
            | Left -> TacticType.Left
            | Right -> TacticType.Right