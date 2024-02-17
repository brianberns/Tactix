namespace Tactix

// https://stackoverflow.com/questions/64929689/avoiding-the-error-where-a-module-and-a-type-definition-occur-in-two-parts-of-an
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TacticType =

    let emoji = function
        | TacticType.Exact -> "❤️"
        | TacticType.Intro -> "🚀"
        | TacticType.Apply -> "👣"
        | TacticType.Cases -> "🔪"
        | TacticType.Left -> "👈🏾"
        | TacticType.Right -> "👉🏾"

    let instructions = function
        | TacticType.Exact -> "Drag onto a symbol that matches the goal"
        | TacticType.Intro -> "Drag onto an arrow goal to simplify it"
        | TacticType.Apply -> "Drag onto ▢→■ when the goal is ■ to change the goal to ▢"
        | TacticType.Cases -> "Drag onto ∧ or ∨ in the field to split them"
        | TacticType.Left -> "Drag onto a ∨ goal to choose its left symbol"
        | TacticType.Right -> "Drag onto a ∨ goal to choose its right symbol"

type Level =
    {
        Goal : Type
        Terms : Set<Term>
        TacticTypes : Set<TacticType>
        Instructions : string
    }

module Level =

    let private p = Primitive "P"
    let private q = Primitive "Q"
    let private r = Primitive "R"

    let private pq = Function (p, q)
    let private pqr = Function (p, Function (q, r))

    let private p_and_q = Product [p; q]
    let private p_or_q = Sum [p; q]

    let private exact = TacticType.emoji TacticType.Exact
    let private intro = TacticType.emoji TacticType.Intro
    let private apply = TacticType.emoji TacticType.Apply
    let private cases = TacticType.emoji TacticType.Cases
    let private left = TacticType.emoji TacticType.Left
    let private right = TacticType.emoji TacticType.Right

    let private level1 =
        {
            Goal = p
            Terms =
                set [
                    Term.create p
                    Term.create q
                ]
            TacticTypes = set [ TacticType.Exact ]
            Instructions =
                $"Drag {exact} onto the symbol that matches the top goal"
        }

    let private level2 =
        {
            Goal = r
            Terms =
                set [
                    Term.create p
                    Term.create q
                    Term.create r
                ]
            TacticTypes = set [ TacticType.Exact ]
            Instructions = ""
        }

    let private level3 =
        {
            Goal = pq
            Terms =
                set [
                    Term.create p
                    Term.create q
                    Term.create pq
                ]
            TacticTypes = set [ TacticType.Exact ]
            Instructions = $"You can also use {exact} on more complex symbols"
        }

    let private level4 =
        {
            Goal = pq
            Terms = set [ Term.create q ]
            TacticTypes =
                set [
                    TacticType.Exact
                    TacticType.Intro
                ]
            Instructions = $"Drag {intro} onto an arrow goal to simplify it"
        }

    let private level5 =
        {
            Goal = Function (p, p)
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
            Goal = pqr
            Terms = set [ Term.create r ]
            TacticTypes =
                set [
                    TacticType.Exact
                    TacticType.Intro
                ]
            Instructions = ""
        }

    let private level7 =
        {
            Goal = q
            Terms =
                set [
                    Term.create p
                    Term.create pq
                ]
            TacticTypes =
                set [
                    TacticType.Exact
                    TacticType.Intro
                    TacticType.Apply
                ]
            Instructions = $"Drag {apply} onto ▢→■ when the goal is ■ to change the goal to ▢"
        }

    let private level8 =
        {
            Goal = Function (p, r)
            Terms =
                set [
                    Term.create (Function (p, q))
                    Term.create (Function (q, r))
                ]
            TacticTypes =
                set [
                    TacticType.Exact
                    TacticType.Intro
                    TacticType.Apply
                ]
            Instructions = ""
        }

    let private level9 =
        {
            Goal = p
            Terms =
                set [
                    Term.create (Product [p; q])
                ]
            TacticTypes =
                set [
                    TacticType.Exact
                    TacticType.Intro
                    TacticType.Apply
                    TacticType.Cases
                ]
            Instructions = $"Drag {cases} onto ∧ in the field to split it"
        }
 
    let private level10 =
        {
            Goal = r
            Terms =
                set [
                    Term.create p_or_q
                    Term.create (Function (p, r))
                    Term.create (Function (q, r))
                ]
            TacticTypes =
                set [
                    TacticType.Exact
                    TacticType.Intro
                    TacticType.Apply
                    TacticType.Cases
                ]
            Instructions = $"Drag {cases} onto ∨ in the field to split it"
        }

    let private level11 =
        {
            Goal = Sum [q; p]
            Terms = set [ Term.create p_or_q ]
            TacticTypes =
                set [
                    TacticType.Exact
                    TacticType.Intro
                    TacticType.Apply
                    TacticType.Cases
                    TacticType.Left
                    TacticType.Right
                ]
            Instructions = $"Drag {left}/{right} onto a ∨ goal to simplify it"
        }

    (*
    // https://www.wikiwand.com/en/Absorption_(logic)
    let private level12 =
        {
            Goal = pqr
            Terms =
                set [
                    Term.create (Function (p_and_q, r))
                ]
            TacticTypes =
                set [
                    TacticType.Exact
                    TacticType.Intro
                    TacticType.Apply
                    TacticType.Cases
                    TacticType.Left
                    TacticType.Right
                ]
            Instructions = ""
        }
    *)

    let levels =
        [|
            level1
            level2
            level3
            level4
            level5
            level6
            level7
            level8
            level9
            level10
            level11
            // level12
        |]

    let initializeProof level =
        let case =
            {
                GoalOpt = Some level.Goal
                Terms = level.Terms
            }
        Proof.empty
            |> Proof.add case
