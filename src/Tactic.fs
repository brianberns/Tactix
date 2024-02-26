﻿namespace Tactix

/// Tactic types.
[<RequireQualifiedAccess>]
type TacticType =
    | Exact
    | Intro
    | Apply
    | Dissolve
    | Cases
    | Left
    | Right
    | Split

/// A tactic used in a proof. Some of these are from the Lean
/// language.
// https://www.ma.imperial.ac.uk/~buzzard/lean_together/source/contents.html
type Tactic =

    /// Term (HP : P) completes goal P.
    | Exact of Term

    /// Introduces a term (HP : P) for goal P -> Q, changing the
    /// goal to just Q.
    | Intro of goal : Type

    /// Applies term (P1 -> P2 -> ... -> PN -> Q), where the
    /// goal is Q, replacing the goal with (P1 ∧ P2 ... ∧ PN).
    /// (This behavior is slightly different from Lean, which
    /// replaces the goal with N separate goals, P1 through PN.)
    | Apply of Term

    /// Dissolves term (HPQ : P ∧ Q) into (HP : P) and (HQ : Q).
    | Dissolve of Term

    /// Breaks up term (HPQ : P ∨ Q) into separate cases for
    /// (HP : P) and (HQ : Q).
    | Cases of Term

    /// Changes goal (P ∨ Q) to just P.
    | Left of goal : Type

    /// Changes goal (P ∨ Q) to just Q.
    | Right of goal : Type

    /// Splits goal (P ∧ Q) into two cases, P and Q.
    | Split of goal : Type

    /// Type of this tactic.
    member tactic.Type =
        match tactic with
            | Exact _    -> TacticType.Exact
            | Intro _    -> TacticType.Intro
            | Apply _    -> TacticType.Apply
            | Dissolve _ -> TacticType.Dissolve
            | Cases _    -> TacticType.Cases
            | Left _     -> TacticType.Left
            | Right _    -> TacticType.Right
            | Split _    -> TacticType.Split
