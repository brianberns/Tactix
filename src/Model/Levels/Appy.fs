namespace Tactix

module Apply =

    open LevelBuilder

    let goalTactics = Dissolve.goalTactics
    let termTactics =
        Dissolve.termTactics + set [ TacticType.Apply ]

    /// Modus ponens.
    let level1 =
        {
            Goal = q
            Terms = terms [p; pq]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = $"Drag {TacticType.apply} onto ▨{Text.implies}■ when the goal is ■ to change the goal to ▨"
        }

    /// Implication is transitive.
    let level2 =
        {
            Goal = Function (p, r)
            Terms =
                terms [
                    Function (p, q)
                    Function (q, r)
                ]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }

    /// Currying.
    let level3 =
        {
            Goal = Function (p_and_q, r)
            Terms = terms [pqr]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = $"You can also use {TacticType.apply} on a nested ▤{Text.implies}[▨{Text.implies}■] symbol when the goal is ■ to create a ▤{Text.andSymbol}▨ goal"
        }
