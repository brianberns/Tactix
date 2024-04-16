namespace Tactix

module Dissolve =

    open LevelBuilder

    let goalTactics =
        Intro.goalTactics + set [ TacticType.DissolveGoal ]
    let termTactics =
        Intro.termTactics + set [ TacticType.DissolveTerm ]

    /// Introduces the dissolve goal tactic.
    let level1 =
        {
            Goal = Sum [p; q]
            Terms = terms [p]
            GoalTactics = goalTactics
            TermTactics = Intro.termTactics
            Instruction = TacticType.instruction TacticType.DissolveGoal
        }

    /// (P -> Q) or (Q -> P).
    let level2 =
        {
            Goal = Sum [pq; qp]
            Terms = terms []
            GoalTactics = goalTactics
            TermTactics = Intro.termTactics
            Instruction = ""
        }

    /// Introduces the dissolve term tactic.
    let level3 =
        {
            Goal = p
            Terms = terms [ p_and_q ]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = TacticType.instruction TacticType.DissolveTerm
        }
