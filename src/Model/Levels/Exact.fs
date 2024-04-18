namespace Tactix

module Exact =

    open LevelBuilder

    let goalTactics = set []
    let termTactics = set [ TacticType.Exact ]

    /// Introduces the "exact" tactic.
    let level1 =
        {
            Goals = set [p]
            Terms = terms [p; q]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction =
                $"Drag {TacticType.exact} onto the symbol that matches the top goal"
        }

    /// More practice with "exact".
    let level2 =
        {
            Goals = set [r]
            Terms = terms [p; q; r]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }
