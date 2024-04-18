namespace Tactix

module More =

    open LevelBuilder

    let goalTactics = Negation.goalTactics
    let termTactics = Negation.termTactics

    let level1 =
        {
            Goals = set [ Sum [ pq; Function (p, Not q) ] ]
            Terms = terms []
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }

    let level2 =
        {
            Goals = set [p]
            Terms = terms [ p_or_q; qr; Not r ]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }

    let level3 =
        {
            Goals =
                set [
                    Function (
                        Product [
                            p_or_q
                            Sum [Not p; r]
                        ],
                        Sum [q; r])
                ]
            Terms = terms []
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }

    let level4 =
        {
            Goals =
                set [
                    Function (
                        pq,
                        Function (
                            Function (Not p, q), q))
                ]
            Terms = terms [Sum [p; Not p]]   // should be possible without this
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }

    let level5 =
        {
            Goals = set [q]
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

    let level6 =
        {
            Goals = set [ Sum [ Not p; Not q ] ]
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

    let level7 =
        {
            Goals =
                set [
                    Function (
                        Product [ pq; Function (r, s) ],
                        Function (
                            Sum [p; r],
                            Sum [q; s]))
                ]
            Terms = terms []
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }

    let level8 =
        {
            Goals =
                set [
                    Function (
                        Product [ p_or_q; Sum [Not p; r] ],
                        Sum [ q; r ])
                ]
            Terms = terms []
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }

    let level9 =
        {
            Goals =
                set [
                    Function (
                        Not s,
                        Not (Function (p, qr)))
                ]
            Terms =
                terms [
                    Not r
                    Sum [p_and_q; s]
                ]
            GoalTactics = goalTactics
            TermTactics = termTactics
            Instruction = ""
        }
