namespace Tactix

/// A puzzle to be solved.
type Level =
    {
        /// Proposition to be proved.
        Goal : Type

        /// Hypotheses.
        Terms : Set<Term>

        /// Available goal-level tactics.
        GoalTactics : Set<TacticType>

        /// Available term-level tactics.
        TermTactics : Set<TacticType>

        /// Hint for the user.
        Instruction : string
    }

module Level =

    /// Starts a proof for the given level.
    let initializeProof level =
        let case =
            {
                Goals = Set.singleton level.Goal
                Terms = level.Terms
                IsComplete = false
            }
        Proof.empty
            |> Proof.add None case
