namespace Tactix

module Exact =

    open LevelBuilder

    let goalTactics = set []
    let termTactics = set [ TacticType.Exact ]

    /// Introduces the "exact" tactic.
    let level1 =
        {
            Goal = p
            Terms = terms [p; q]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction =
                $"Drag {TacticType.exact} onto the symbol that matches the top goal"
        }

    /// More practice with "exact".
    let level2 =
        {
            Goal = r
            Terms = terms [p; q; r]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }

    /// Introduces function types.
    let level3 =
        {
            Goal = pq
            Terms = terms [p; q; pq]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = $"You can also use {TacticType.exact} on a more complex symbol, as long as it matches the top goal"
        }
