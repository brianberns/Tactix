namespace Tactix

module More =

    open LevelBuilder

    let goalTactics = Negation.goalTactics
    let termTactics = Negation.termTactics

    let level1 =
        {
            Goal = Sum [ pq; Function (p, Not q) ]
            Terms = terms []
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }

    let level2 =
        {
            Goal = p
            Terms = terms [ p_or_q; qr; Not r ]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }

    let level3 =
        {
            Goal =
                Function (
                    pq,
                    Function (
                        Function (Not p, q), q))
            Terms = terms [Sum [p; Not p]]   // should be possible without this
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }

    let level4 =
        {
            Goal = q
            Terms =
                terms [
                    Product [ pq; Function (r, s) ]
                    Sum [ p; r ]
                    Not s
                ]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }

    let level5 =
        {
            Goal = Sum [ Not p; Not q ]
            Terms =
                terms [
                    Function (p_and_q, r)
                    Function (Sum [r; s], t)
                    Not t
                ]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }

    let level6 =
        {
            Goal =
                Function (
                    Product [ pq; Function (r, s) ],
                    Function (
                        Sum [p; r],
                        Sum [q; s]))
            Terms = terms []
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }
