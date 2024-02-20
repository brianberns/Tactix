namespace Tactix

/// Tactic type.
[<RequireQualifiedAccess>]
type TacticType =
    | Exact
    | Intro
    | Apply
    | Cases
    | Left
    | Right
    | Split

/// A tactic used in a proof, from the Lean language.
// https://www.ma.imperial.ac.uk/~buzzard/lean_together/source/contents.html
type Tactic =

    /// Term (HP : P) eliminates goal P.
    | Exact of Term

    /// Introduces term (HP : P) when goal is P -> Q, changing
    /// goal to just Q.
    | Intro of Term

    /// Applies term (P1 -> P2 -> ... -> PN -> Q), where the
    /// goal is Q, replacing the goal with N separate goals,
    /// P1 through PN.
    | Apply of Term

    /// Breaks up an hypothesis (HP : P) into its component
    /// parts, creating goals for each constructor of P.
    | Cases of Term

    /// Changes goal (P ∨ Q) to just P.
    | Left

    /// Changes goal (P ∨ Q) to just Q.
    | Right

    /// Splits goal (P ∧ Q) into two cases.
    | Split

    /// Tactic type.
    member tactic.Type =
        match tactic with
            | Exact _ -> TacticType.Exact
            | Intro _ -> TacticType.Intro
            | Apply _ -> TacticType.Apply
            | Cases _ -> TacticType.Cases
            | Left -> TacticType.Left
            | Right -> TacticType.Right
            | Split -> TacticType.Split
