﻿namespace Tactix

module Split =

    open LevelBuilder

    let goalTactics =
        Apply.goalTactics + set [ TacticType.SplitGoal ]
    let termTactics =
        Apply.termTactics + set [ TacticType.SplitTerm ]

    /// Commutivity of ∧.
    let level1 =
        {
            Goal = Product [q; p]
            Terms = terms [p_and_q]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = $"Drag {TacticType.splitGoal} onto a {Text.andSymbol} goal to create separate cases"
        }

    /// Commutivity of ∨.
    let level2 =
        {
            Goal = Sum [q; p]
            Terms = terms [p_or_q]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = $"Drag {TacticType.splitTerm} onto a {Text.orSymbol} symbol to create separate cases"
        }

    /// More practice with multiple cases.
    let level3 =
        {
            Goal = r
            Terms =
                terms [
                    p_or_q
                    pr
                    qr
                ]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }

    /// Exportation.
    let level4 =
        {
            Goal = pqr
            Terms = terms [ Function (p_and_q, r) ]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }

    /// Distributive property.
    let level5 =
        {
            Goal =
                Sum [
                    Product [p; r]
                    Product [q; r]
                ]
            Terms =
                terms [
                    Product [Sum [p; q]; r]
                ]
            GoalTactics =
                set [
                    TacticType.Intro
                    TacticType.DissolveGoal
                    TacticType.SplitGoal
                ]
            TermTactics =
                set [
                    TacticType.Exact
                    TacticType.Apply
                    TacticType.DissolveTerm
                    TacticType.SplitTerm
                ]
            Instruction = ""
        }