namespace Tactix

/// Tactic types.
[<RequireQualifiedAccess>]
type TacticType =
    | Exact
    | Intro
    | Apply
    | DissolveGoal
    | DissolveTerm
    | SplitGoal
    | SplitTerm
    | AffirmGoal
    | AffirmTerm
    | AddZero
    | AddSucc

/// A tactic used in a proof. Some of these are from the Lean
/// language.
// https://www.ma.imperial.ac.uk/~buzzard/lean_together/source/contents.html
type Tactic =

    /// Term (HP : P) completes goal P.
    | Exact of Term

    /// Introduces a Boolean term (HP : P) for goal P -> Q, changing
    /// the goal to just Q.
    | Intro of goal : Boolean

    /// Applies term (P1 -> P2 -> ... -> PN -> Q), where the
    /// goal is Q, replacing the goal with (P1 ∧ P2 ... ∧ PN).
    /// (This behavior is slightly different from Lean, which
    /// replaces the goal with N separate goals, P1 through PN.)
    | Apply of Term

    /// Dissolves goal (P ∨ Q) into separate goals P and Q.
    | DissolveGoal of goal : Boolean

    /// Dissolves term (HPQ : P ∧ Q) into (HP : P) and (HQ : Q).
    | DissolveTerm of Term

    /// Splits goal (P ∧ Q) into separate cases for P and Q.
    | SplitGoal of goal : Boolean

    /// Splits term (HPQ : P ∨ Q) into separate cases for
    /// (HP : P) and (HQ : Q).
    | SplitTerm of Term

    /// Converts goal ¬P to term (HP : P).
    | AffirmGoal of goal : Boolean

    /// Converts term (HNP : ¬P) to goal P.
    | AffirmTerm of Term

    | AddZero of Term
    | AddSuccessor of Term

    /// Type of this tactic.
    member tactic.Type =
        match tactic with
            | Exact _        -> TacticType.Exact
            | Intro _        -> TacticType.Intro
            | Apply _        -> TacticType.Apply
            | DissolveGoal _ -> TacticType.DissolveGoal
            | DissolveTerm _ -> TacticType.DissolveTerm
            | SplitTerm _    -> TacticType.SplitTerm
            | SplitGoal _    -> TacticType.SplitGoal
            | AffirmGoal _   -> TacticType.AffirmGoal
            | AffirmTerm _   -> TacticType.AffirmTerm
            | AddZero _      -> TacticType.AddZero
            | AddSuccessor _ -> TacticType.AddSucc
