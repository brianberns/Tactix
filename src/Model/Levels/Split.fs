namespace Tactix

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
            TermTactics = Apply.termTactics
            Instruction = TacticType.instruction TacticType.SplitGoal
        }

    /// Commutivity of ∨.
    let level2 =
        {
            Goal = Sum [q; p]
            Terms = terms [p_or_q]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = TacticType.instruction TacticType.SplitTerm
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

    /// Exportation or uncurrying.
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

    /// Currying.
    let level6 =
        {
            Goal = Function (p_and_q, r)
            Terms = terms [pqr]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = $"You can also use {TacticType.apply} on a nested ▤{Text.implies}[▨{Text.implies}■] symbol when the goal is ■ to create a ▤{Text.andSymbol}▨ goal"
        }
