namespace Tactix

module Tautology =

    open LevelBuilder

    let goalTactics = Negation.goalTactics
    let termTactics = Negation.termTactics

    let level1 =
        {
            Goal = Sum [pq; Function (p, Not q)]
            Terms = terms []
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }

    let level2 =
        {
            Goal = Function (pq, Function (Function (Not p, q), q))
            Terms = terms [Sum [p; Not p]]   // should be possible without this
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }
