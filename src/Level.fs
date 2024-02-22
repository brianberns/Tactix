namespace Tactix

/// Action type.
[<RequireQualifiedAccess>]
type ActionType =
    | Exact
    | Intro
    | Apply
    | Cases
    | Left
    | Right
    | Split
    | Expand

module ActionType =

    let emoji = function
        | ActionType.Exact  -> "❤️"
        | ActionType.Intro  -> "🚀"
        | ActionType.Apply  -> "👣"
        | ActionType.Cases  -> "🔪"
        | ActionType.Left   -> "👈🏾"
        | ActionType.Right  -> "👉🏾"
        | ActionType.Split  -> "🌈"
        | ActionType.Expand -> "🧣"

    let instructions = function
        | ActionType.Exact  -> "Drag onto a symbol that matches the goal"
        | ActionType.Intro  -> "Drag onto an arrow goal to simplify it"
        | ActionType.Apply  -> "Drag onto ▢→■ when the goal is ■ to change the goal to ▢"
        | ActionType.Cases  -> "Drag onto ∧ or ∨ in the field to split them"
        | ActionType.Left   -> "Drag onto a ∨ goal to choose its left symbol"
        | ActionType.Right  -> "Drag onto a ∨ goal to choose its right symbol"
        | ActionType.Split  -> "Drag onto a ∧ goal to split it"
        | ActionType.Expand -> "Drag anywhere to expand ¬ symbols"

/// A puzzle to be solved.
type Level =
    {
        /// Proposition to be proved.
        Goal : Type

        /// Hypotheses.
        Terms : Set<Term>

        /// Available actions.
        ActionTypes : Set<ActionType>

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

    let private exact  = ActionType.emoji ActionType.Exact
    let private intro  = ActionType.emoji ActionType.Intro
    let private apply  = ActionType.emoji ActionType.Apply
    let private cases  = ActionType.emoji ActionType.Cases
    let private left   = ActionType.emoji ActionType.Left
    let private right  = ActionType.emoji ActionType.Right
    let private split  = ActionType.emoji ActionType.Split
    let private expand = ActionType.emoji ActionType.Expand

    /// Builds terms from types.
    let private terms types =
        types
            |> Seq.map Term.create
            |> set

    module private Exact =

        let private actionTypes = set [ ActionType.Exact ]

        /// Introduces the "exact" tactic.
        let level1 =
            {
                Goal = p
                Terms = terms [p; q]
                ActionTypes = actionTypes
                Instructions =
                    $"Drag {exact} onto the symbol that matches the top goal"
            }

        /// More practice with "exact".
        let level2 =
            {
                Goal = r
                Terms = terms [p; q; r]
                ActionTypes = actionTypes
                Instructions = ""
            }

        /// Introduces function types.
        let level3 =
            {
                Goal = pq
                Terms = terms [p; q; pq]
                ActionTypes = actionTypes
                Instructions = $"You can also use {exact} on more complex symbols"
            }

    module private Intro =

        let private actionTypes =
            set [
                ActionType.Exact
                ActionType.Intro
            ]

        /// Introduces the "intro" tactic with Q ⊢ P → Q.
        let level1 =
            {
                Goal = pq
                Terms = terms [q]
                ActionTypes = actionTypes
                Instructions = $"Drag {intro} onto an arrow goal to simplify it"
            }

        /// P → P.
        let level2 =
            {
                Goal = Function (p, p)
                Terms = terms []
                ActionTypes = actionTypes
                Instructions = ""
            }

        /// More practice with intro.
        let level3 =
            {
                Goal = pqr
                Terms = terms [r]
                ActionTypes = actionTypes
                Instructions = ""
            }

    module private Apply =

        let private actionTypes =
            set [
                ActionType.Exact
                ActionType.Intro
                ActionType.Apply
            ]

        /// Modus ponens.
        let level1 =
            {
                Goal = q
                Terms = terms [p; pq]
                ActionTypes = actionTypes
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
                ActionTypes = actionTypes
                Instructions = ""
            }

    module Split =

        let actionTypes =
            set [
                ActionType.Exact
                ActionType.Intro
                ActionType.Apply
                ActionType.Cases
                ActionType.Left
                ActionType.Right
                ActionType.Split
                ActionType.Expand
            ]

        /// Commutivity of ∧.
        let level1 =
            {
                Goal = Product [q; p]
                Terms = terms [ Product [p; q] ]
                ActionTypes = actionTypes
                Instructions = $"Drag {split} onto a ∧ goal to split it"
            }

        /// Applying a function with two inputs.
        let level2 =
            {
                Goal = r
                Terms = terms [p; q; pqr]
                ActionTypes = actionTypes
                Instructions = $"Use {apply} on nested ▢→■ symbols when the goal is ■"
            }

        /// Exportation.
        let level3 =
            {
                Goal = pqr
                Terms = terms [ Function (p_and_q, r) ]
                ActionTypes = actionTypes
                Instructions = ""
            }

        /// Distributive property.
        let level4 =
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
                ActionTypes = actionTypes
                Instructions = ""
            }

    module private Cases =

        let private actionTypes =
            set [
                ActionType.Exact
                ActionType.Intro
                ActionType.Apply
                ActionType.Cases
                ActionType.Left
                ActionType.Right
                ActionType.Split
                ActionType.Expand
            ]


        /// Splitting a ∧ term with the "cases" tactic.
        let level1 =
            {
                Goal = p
                Terms = terms [ Product [p; q] ]
                ActionTypes = actionTypes
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
                ActionTypes = actionTypes
                Instructions = $"Drag {cases} onto ∨ in the field to split it"
            }

        /// Currying.
        let level3 =
            {
                Goal = Function (p_and_q, r)
                Terms = terms [pqr]
                ActionTypes = actionTypes
                Instructions = ""
            }

    module private LeftRight =

        let private actionTypes =
            set [
                ActionType.Exact
                ActionType.Intro
                ActionType.Apply
                ActionType.Cases
                ActionType.Left
                ActionType.Right
                ActionType.Split
                ActionType.Expand
            ]

        /// Commutivity of ∨.
        let level1 =
            {
                Goal = Sum [q; p]
                Terms = terms [p_or_q]
                ActionTypes = actionTypes
                Instructions = $"Drag {left}/{right} onto a ∨ goal to simplify it"
            }

    module private Negation =

        let actionTypes =
            set [
                ActionType.Exact
                ActionType.Intro
                ActionType.Apply
                ActionType.Cases
                ActionType.Left
                ActionType.Right
                ActionType.Split
                ActionType.Expand
            ]

        /// Double negative (but not the law of excluded middle).
        let level1 =
            {
                Goal = Type.not (Type.not p)
                Terms = terms [ p ]
                ActionTypes = actionTypes
                Instructions = $"Drag {expand} anywhere to expand ¬ symbols"
            }

        /// de Morgan's laws.
        let level2 =
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
                ActionTypes = actionTypes
                Instructions = ""
            }

        /// de Morgan's laws.
        let level3 =
            {
                Goal = Type.not (Product [p; q])
                Terms =
                    terms [
                        Sum [
                            Type.not p
                            Type.not q
                        ]
                    ]
                ActionTypes = actionTypes
                Instructions = ""
            }

        /// Modus tollens.
        let level4 =
            {
                Goal = Function (Type.not q, Type.not p)
                Terms = terms [ pq ]
                ActionTypes = actionTypes
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

            Split.level1
            Split.level2
            Split.level3
            Split.level4

            Cases.level1
            Cases.level2
            Cases.level3

            LeftRight.level1

            Negation.level1
            Negation.level2
            Negation.level3
            Negation.level4
        |]

    let initializeProof level =
        let case =
            {
                GoalOpt = Some level.Goal
                Terms = level.Terms
            }
        Proof.empty
            |> Proof.add case
