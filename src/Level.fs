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
                    Term.create "HP" typeP
                    Term.create "HQ" typeQ
                ]
            TacticTypes =
                set [
                    TacticType.Exact
                ]
            Instructions =
                $"Drag {exact} onto the symbol that matches the top goal."
        }

    let private level2 =
        {
            Goal = typeR
            Terms =
                set [
                    Term.create "HP" typeP
                    Term.create "HQ" typeQ
                    Term.create "HR" typeR
                ]
            TacticTypes =
                set [
                    TacticType.Exact
                ]
            Instructions = ""
        }

    let private typePQ = Function (typeP, typeQ)

    let private level3 =
        {
            Goal = typePQ
            Terms =
                set [
                    Term.create "HP" typeP
                    Term.create "HQ" typeQ
                    Term.create "HPQ" typePQ
                ]
            TacticTypes =
                set [
                    TacticType.Exact
                ]
            Instructions = $"{exact} also works on more more complex symbols"
        }

    let private level4 =
        {
            Goal = typePQ
            Terms =
                set [
                    Term.create "HQ" typeQ
                ]
            TacticTypes =
                set [
                    TacticType.Exact
                    TacticType.Intro
                ]
            Instructions = $"Drag {intro} onto the goal to simplify it"
        }

    let levels =
        [|
            level1
            level2
            level3
            level4
        |]

    let initializeProof level =
        {
            Goal = Some level.Goal
            Terms = level.Terms
        }
