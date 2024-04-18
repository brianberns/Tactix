namespace Tactix

module Apply =

    open LevelBuilder

    let goalTactics = Dissolve.goalTactics
    let termTactics =
        Dissolve.termTactics + set [ TacticType.Apply ]

    /// Modus ponens.
    let level1 =
        {
            Goals = set [q]
            Terms = terms [p; pq]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = $"Drag {TacticType.apply} onto ▨{Text.implies}■ when the goal is ■ to change the goal to ▨"
        }

    /// Implication is transitive.
    let level2 =
        {
            Goals = set [ Function (p, r) ]
            Terms =
                terms [
                    Function (p, q)
                    Function (q, r)
                ]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }
