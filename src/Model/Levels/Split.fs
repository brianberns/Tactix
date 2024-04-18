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
            Goals = set [ Product [q; p] ]
            Terms = terms [p_and_q]
            GoalTactics = goalTactics
            TermTactics = Apply.termTactics
            Instruction = TacticType.instruction TacticType.SplitGoal
        }

    /// Commutivity of ∨.
    let level2 =
        {
            Goals = set [ Sum [q; p] ]
            Terms = terms [p_or_q]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = TacticType.instruction TacticType.SplitTerm
        }

    /// More practice with multiple cases.
    let level3 =
        {
            Goals = set [r]
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
            Goals = set [pqr]
            Terms = terms [ Function (p_and_q, r) ]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }

    /// Distributive property.
    let level5 =
        {
            Goals =
                set [
                    Sum [
                        Product [p; r]
                        Product [q; r]
                    ]
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
            Goals = set [ Function (p_and_q, r) ]
            Terms = terms [pqr]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = $"You can also use {TacticType.apply} on a nested ▤{Text.implies}[▨{Text.implies}■] symbol when the goal is ■ to create a ▤{Text.andSymbol}▨ goal"
        }
