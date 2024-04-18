namespace Tactix

/// A puzzle to be solved.
type Level =
    {
        /// Propositions to be proved.
        Goals : Set<Type>

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
                Goals = level.Goals
                Terms = level.Terms
                IsComplete = false
            }
        Proof.empty
            |> Proof.add None case
