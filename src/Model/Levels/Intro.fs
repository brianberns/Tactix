namespace Tactix

module Intro =

    open LevelBuilder

    let goalTactics =
        Exact.goalTactics + set [ TacticType.Intro ]
    let termTactics = Exact.termTactics

    /// Introduces the "intro" tactic with Q ⊢ P → Q.
    let level1 =
        {
            Goal = pq
            Terms = terms [q]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = $"Drag {TacticType.intro} onto a {Text.implies} goal to simplify it"
        }

    /// P → P.
    let level2 =
        {
            Goal = Function (p, p)
            Terms = terms []
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }

    /// More practice with intro.
    let level3 =
        {
            Goal = pqr
            Terms = terms [r]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }
