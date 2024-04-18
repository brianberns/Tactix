namespace Tactix

module Intro =

    open LevelBuilder

    let goalTactics =
        Exact.goalTactics + set [ TacticType.Intro ]
    let termTactics = Exact.termTactics

    /// Introduces the "intro" tactic with Q ⊢ P → Q.
    let level1 =
        {
            Goals = set [pq]
            Terms = terms [q]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = TacticType.instruction TacticType.Intro
        }

    /// P → P.
    let level2 =
        {
            Goals = set [Function (p, p)]
            Terms = terms []
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }

    /// More practice with intro.
    let level3 =
        {
            Goals = set [pqr]
            Terms = terms [r]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }
