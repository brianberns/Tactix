namespace Tactix

module Tautology =

    open LevelBuilder

    let goalTactics = Negation.goalTactics
    let termTactics = Negation.termTactics

    let level1 =
        {
            Goal = Function (pq, Function (Function (Not p, q), q))
            Terms = terms [Sum [p; Not p]]   // should be possible without this
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }
