namespace Tactix

[<RequireQualifiedAccess>]
type TacticType =
    | Exact
    | Intro
    | Apply

module TacticType =

    let emoji = function
        | TacticType.Exact -> "🎆"
        | TacticType.Intro -> "🚀"
        | TacticType.Apply -> "👣"

type Level =
    {
        Goal : Type
        Terms : Set<Term>
        TacticTypes : Set<TacticType>
        Instructions : string
    }

module Level =

    let private typeP = Primitive "P"
    let private typeQ = Primitive "Q"
    let private typeR = Primitive "R"

    let private exact = TacticType.emoji TacticType.Exact
    let private intro = TacticType.emoji TacticType.Intro

    let private level1 =
        {
            Goal = typeP
            Terms =
                set [
                    Term.create typeP
                    Term.create typeQ
                ]
            TacticTypes = set [ TacticType.Exact ]
            Instructions =
                $"Drag {exact} onto the symbol that matches the top goal."
        }

    let private level2 =
        {
            Goal = typeR
            Terms =
                set [
                    Term.create typeP
                    Term.create typeQ
                    Term.create typeR
                ]
            TacticTypes = set [ TacticType.Exact ]
            Instructions = ""
        }

    let private typePP = Function (typeP, typeP)
    let private typePQ = Function (typeP, typeQ)
    let private typePQR = Function (typeP, Function (typeQ, typeR))

    let private level3 =
        {
            Goal = typePQ
            Terms =
                set [
                    Term.create typeP
                    Term.create typeQ
                    Term.create typePQ
                ]
            TacticTypes = set [ TacticType.Exact ]
            Instructions = $"You can also use {exact} on more complex symbols"
        }

    let private level4 =
        {
            Goal = typePQ
            Terms = set [ Term.create typeQ ]
            TacticTypes =
                set [
                    TacticType.Exact
                    TacticType.Intro
                ]
            Instructions = $"Drag {intro} onto the goal to simplify it"
        }

    let private level5 =
        {
            Goal = typePP
            Terms = Set.empty
            TacticTypes =
                set [
                    TacticType.Exact
                    TacticType.Intro
                ]
            Instructions = ""
        }

    let private level6 =
        {
            Goal = typePQR
            Terms = set [ Term.create typeR ]
            TacticTypes =
                set [
                    TacticType.Exact
                    TacticType.Intro
                ]
            Instructions = ""
        }

    let levels =
        [|
            level1
            level2
            level3
            level4
            level5
            level6
        |]

    let initializeProof level =
        {
            GoalOpt = Some level.Goal
            Terms = level.Terms
        }
