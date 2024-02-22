namespace Tactix

/// Action type.
[<RequireQualifiedAccess>]
type ActionType =
    | Exact
    | Intro
    | Left
    | Right
    | Apply
    | Cases
    | Split
    | Expand

module ActionType =

    let emoji = function
        | ActionType.Exact  -> "❤️"
        | ActionType.Intro  -> "🚀"
        | ActionType.Left   -> "👈🏾"
        | ActionType.Right  -> "👉🏾"
        | ActionType.Apply  -> "👣"
        | ActionType.Cases  -> "🔪"
        | ActionType.Split  -> "🌈"
        | ActionType.Expand -> "🧣"

    let instructions = function
        | ActionType.Exact  -> "Drag onto a symbol that matches the goal"
        | ActionType.Intro  -> "Drag onto an arrow goal to simplify it"
        | ActionType.Apply  -> "Drag onto ▢→■ when the goal is ■ to change the goal to ▢"
        | ActionType.Cases  -> "Drag onto ∧ or ∨ in the field to split them"
        | ActionType.Left   -> "Drag onto a ∨ goal to choose its left symbol"
        | ActionType.Right  -> "Drag onto a ∨ goal to choose its right symbol"
        | ActionType.Split  -> "Drag onto a ∧ symbol to dissolve it"
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
    let private qr = Function (q, r)
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

        /// Introduces the "exact" action.
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

        /// Introduces the "intro" action with Q ⊢ P → Q.
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

    module private LeftRight =

        let private actionTypes =
            set [
                ActionType.Exact
                ActionType.Intro
                ActionType.Left
                ActionType.Right
            ]

        /// Introduces the left action.
        let level1 =
            {
                Goal = Sum [p; q]
                Terms = terms [p]
                ActionTypes = actionTypes
                Instructions = $"Drag {left}/{right} onto a ∨ goal to simplify it"
            }

        /// Introduces the right action.
        let level2 =
            {
                Goal = Sum [p; qr]
                Terms = terms [qr]
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

        /// Introduces the split action.
        let level1 =
            {
                Goal = p
                Terms = terms [ p_and_q ]
                ActionTypes =
                    set [
                        ActionType.Exact
                        ActionType.Split
                    ]
                Instructions = $"Drag {split} onto a ∧ symbol to dissolve it"
            }

    module private Cases =

        /// Commutivity of ∨.
        let level1 =
            {
                Goal = Sum [q; p]
                Terms = terms [p_or_q]
                ActionTypes =
                    set [
                        ActionType.Exact
                        ActionType.Left
                        ActionType.Right
                        ActionType.Cases
                    ]
                Instructions = $"Drag {cases} onto ∨ in the field to create separate cases"
            }

        /// More practice with multiple cases.
        let level2 =
            {
                Goal = r
                Terms =
                    terms [
                        p_or_q
                        Function (p, r)
                        Function (q, r)
                    ]
                ActionTypes =
                    set [
                        ActionType.Exact
                        ActionType.Apply
                        ActionType.Cases
                    ]
                Instructions = ""
            }

    module private Other =

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

        /// Applying a function with two inputs.
        let level1 =
            {
                Goal = r
                Terms = terms [p; q; pqr]
                ActionTypes = actionTypes
                Instructions = $"Use {apply} on nested ▢→■ symbols when the goal is ■"
            }

        /// Currying.
        let level2 =
            {
                Goal = Function (p_and_q, r)
                Terms = terms [pqr]
                ActionTypes =
                    set [
                        ActionType.Exact
                        ActionType.Intro
                        ActionType.Apply
                        ActionType.Split
                    ]
                Instructions = ""
            }

        /// Commutivity of ∧.
        let level4 =
            {
                Goal = Product [q; p]
                Terms = terms [ p_and_q ]
                ActionTypes =
                    set [
                        ActionType.Exact
                        ActionType.Left
                        ActionType.Right
                        ActionType.Split
                    ]
                Instructions = ""
            }

        /// Exportation.
        let level5 =
            {
                Goal = pqr
                Terms = terms [ Function (p_and_q, r) ]
                ActionTypes = actionTypes
                Instructions = ""
            }

        /// Distributive property.
        let level6 =
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
                Goal = Type.not p_and_q
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

            LeftRight.level1
            LeftRight.level2

            Apply.level1
            Apply.level2

            Split.level1

            Cases.level1
            Cases.level2

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
