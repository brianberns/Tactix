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
        | TacticType.Split -> "🎳"

    let instructions = function
        | TacticType.Exact -> "Drag onto a symbol that matches the goal"
        | TacticType.Intro -> "Drag onto an arrow goal to simplify it"
        | TacticType.Apply -> "Drag onto ▢→■ when the goal is ■ to change the goal to ▢"
        | TacticType.Cases -> "Drag onto ∧ or ∨ in the field to split them"
        | TacticType.Left -> "Drag onto a ∨ goal to choose its left symbol"
        | TacticType.Right -> "Drag onto a ∨ goal to choose its right symbol"
        | TacticType.Split -> "Drag onto a ∧ goal to split it"

/// A puzzle to be solved.
type Level =
    {
        /// Proposition to be proved.
        Goal : Type

        /// Hypotheses.
        Terms : Set<Term>

        /// Available actions.
        TacticTypes : Set<TacticType>

        /// Hint for the user.
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
    let private split = TacticType.emoji TacticType.Split

    /// Builds terms from types.
    let private terms types =
        types
            |> Seq.map Term.create
            |> set

    module private Exact =

        /// Introduces the "exact" tactic.
        let level1 =
            {
                Goal = p
                Terms = terms [p; q]
                TacticTypes = set [ TacticType.Exact ]
                Instructions =
                    $"Drag {exact} onto the symbol that matches the top goal"
            }

        /// More practice with "exact".
        let level2 =
            {
                Goal = r
                Terms = terms [p; q; r]
                TacticTypes = set [ TacticType.Exact ]
                Instructions = ""
            }

        /// Introduces function types.
        let level3 =
            {
                Goal = pq
                Terms = terms [p; q; pq]
                TacticTypes = set [ TacticType.Exact ]
                Instructions = $"You can also use {exact} on more complex symbols"
            }

    module private Intro =

        /// Introduces the "intro" tactic with Q ⊢ P → Q.
        let level1 =
            {
                Goal = pq
                Terms = terms [q]
                TacticTypes =
                    set [
                        TacticType.Exact
                        TacticType.Intro
                    ]
                Instructions = $"Drag {intro} onto an arrow goal to simplify it"
            }

        /// P → P.
        let level2 =
            {
                Goal = Function (p, p)
                Terms = terms []
                TacticTypes =
                    set [
                        TacticType.Exact
                        TacticType.Intro
                    ]
                Instructions = ""
            }

        /// More practice with intro.
        let level3 =
            {
                Goal = pqr
                Terms = terms [r]
                TacticTypes =
                    set [
                        TacticType.Exact
                        TacticType.Intro
                    ]
                Instructions = ""
            }

    module private Apply =

        /// Modus ponens.
        let level1 =
            {
                Goal = q
                Terms = terms [p; pq]
                TacticTypes =
                    set [
                        TacticType.Exact
                        TacticType.Intro
                        TacticType.Apply
                    ]
                Instructions = $"Drag {apply} onto ▢→■ when the goal is ■ to change the goal to ▢"
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
                TacticTypes =
                    set [
                        TacticType.Exact
                        TacticType.Intro
                        TacticType.Apply
                    ]
                Instructions = ""
            }

        /// Appling a function with two inputs results in two cases.
        let level3 =
            {
                Goal = r
                Terms = terms [p; q; pqr]
                TacticTypes =
                    set [
                        TacticType.Exact
                        TacticType.Intro
                        TacticType.Apply
                    ]
                Instructions = $"You can also use {apply} on nested ▢→■ symbols when the goal is ■"
            }

    module private Cases =

        /// Splitting a ∧ term with the "cases" tactic.
        let level1 =
            {
                Goal = p
                Terms = terms [ Product [p; q] ]
                TacticTypes =
                    set [
                        TacticType.Exact
                        TacticType.Intro
                        TacticType.Apply
                        TacticType.Cases
                    ]
                Instructions = $"Drag {cases} onto ∧ in the field to split it"
            }
 
        /// Splitting a ∨ term with the "cases" tactic.
        let level2 =
            {
                Goal = r
                Terms =
                    terms [
                        p_or_q
                        Function (p, r)
                        Function (q, r)
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

        /// Currying.
        let level3 =
            {
                Goal = Function (p_and_q, r)
                Terms = terms [pqr]
                TacticTypes =
                    set [
                        TacticType.Exact
                        TacticType.Intro
                        TacticType.Apply
                        TacticType.Cases
                    ]
                Instructions = ""
            }

    module private LeftRight =

        /// Commutivity of ∨.
        let level13 =
            {
                Goal = Sum [q; p]
                Terms = terms [p_or_q]
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

    module Split =

        /// Commutivity of ∧.
        let level1 =
            {
                Goal = Product [q; p]
                Terms = terms [ Product [p; q] ]
                TacticTypes =
                    set [
                        TacticType.Exact
                        TacticType.Intro
                        TacticType.Apply
                        TacticType.Cases
                        TacticType.Left
                        TacticType.Right
                        TacticType.Split
                    ]
                Instructions = $"Drag {split} onto a ∧ goal to split it"
            }

    let private level15 =
        {
            Goal = pqr
            Terms = terms [ Function (p_and_q, r) ]
            TacticTypes =
                set [
                    TacticType.Exact
                    TacticType.Intro
                    TacticType.Apply
                    TacticType.Cases
                    TacticType.Left
                    TacticType.Right
                    TacticType.Split
                ]
            Instructions = ""
        }

    let private level16 =
        {
            Goal =
                Sum [
                    Product [p; r]
                    Product [q; r]
                ]
            Terms =
                terms [
                    Product [Sum [p; q]; r]
                ]
            TacticTypes =
                set [
                    TacticType.Exact
                    TacticType.Intro
                    TacticType.Apply
                    TacticType.Cases
                    TacticType.Left
                    TacticType.Right
                    TacticType.Split
                ]
            Instructions = ""
        }

    let private level17 =
        {
            Goal = Type.not (Type.not p)
            Terms = terms [ p ]
            TacticTypes =
                set [
                    TacticType.Exact
                    TacticType.Intro
                    TacticType.Apply
                    TacticType.Cases
                    TacticType.Left
                    TacticType.Right
                    TacticType.Split
                ]
            Instructions = ""
        }

    let private level18 =
        {
            Goal =
                Product [
                    Type.not p
                    Type.not q
                ]
            Terms =
                terms [
                    Type.not (Sum [p; q])
                ]
            TacticTypes =
                set [
                    TacticType.Exact
                    TacticType.Intro
                    TacticType.Apply
                    TacticType.Cases
                    TacticType.Left
                    TacticType.Right
                    TacticType.Split
                ]
            Instructions = ""
        }

    let private level19 =
        {
            Goal = Type.not (Product [p; q])
            Terms =
                terms [
                    Sum [
                        Type.not p
                        Type.not q
                    ]
                ]
            TacticTypes =
                set [
                    TacticType.Exact
                    TacticType.Intro
                    TacticType.Apply
                    TacticType.Cases
                    TacticType.Left
                    TacticType.Right
                    TacticType.Split
                ]
            Instructions = ""
        }

    let private level20 =
        {
            Goal = Function (Type.not q, Type.not p)
            Terms = terms [ pq ]
            TacticTypes =
                set [
                    TacticType.Exact
                    TacticType.Intro
                    TacticType.Apply
                    TacticType.Cases
                    TacticType.Left
                    TacticType.Right
                    TacticType.Split
                ]
            Instructions = ""
        }

    let levels =
        [|
            Exact.level1
            Exact.level2
            Exact.level3

            Intro.level1
            Intro.level2
            Intro.level3

            Apply.level1
            Apply.level2
            Apply.level3

            Cases.level1
            Cases.level2
            Cases.level3

            LeftRight.level13

            level15
            level16
            level17
            level18
            level19
            level20
        |]

    let initializeProof level =
        let case =
            {
                GoalOpt = Some level.Goal
                Terms = level.Terms
            }
        Proof.empty
            |> Proof.add case
