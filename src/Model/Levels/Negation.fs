namespace Tactix

module Negation =

    open LevelBuilder

    let goalTactics =
        Split.goalTactics + set [ TacticType.AffirmGoal ]
    let termTactics =
        Split.termTactics + set [ TacticType.AffirmTerm ]

    /// Double negative (but not the law of excluded middle).
    let level1 =
        {
            Goal = Not (Not p)
            Terms = terms [p]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = $"Drag {TacticType.affirmGoal} onto a {Text.notSymbol} to remove it"
        }

    /// Modus tollens.
    let level2 =
        {
            Goal = Function (Not q, Not p)
            Terms = terms [ pq ]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }

    /// de Morgan's laws.
    let level3 =
        {
            Goal = Not p_and_q
            Terms =
                terms [
                    Sum [
                        Not p
                        Not q
                    ]
                ]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }

    /// de Morgan's laws.
    let level4 =
        {
            Goal =
                Product [
                    Not p
                    Not q
                ]
            Terms =
                terms [
                    Not (Sum [p; q])
                ]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }
