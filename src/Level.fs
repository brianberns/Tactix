namespace Tactix

module Text =

    let andSymbol = "🐛"
    let orSymbol = "👈🏾👉🏾"
    let notSymbol = "¬"

    let andHtml = andSymbol
    let orHtml = "👈🏾<br />👉🏾"
    let notHtml = notSymbol

/// Goal actions available to the user. These have a mapping
/// to tactics that is not 1:1.
[<RequireQualifiedAccess>]
type GoalAction =

    /// Introduces a term.
    | Intro

    /// Chooses the left sub-goal.
    | Left

    /// Chooses the right sub-goal.
    | Right

    /// Splits a case into multiple sub-cases.
    | Cases

    /// Expands a negation.
    | Expand

module GoalAction =

    let emoji = function
        | GoalAction.Intro    -> "🚀"
        | GoalAction.Left     -> "👈🏾"
        | GoalAction.Right    -> "👉🏾"
        | GoalAction.Cases    -> "🐞"
        | GoalAction.Expand   -> "🧣"

    let instructions = function
        | GoalAction.Intro    -> "Drag onto an arrow goal to simplify it"
        | GoalAction.Left     -> $"Drag onto a {Text.orSymbol} goal to choose its left symbol"
        | GoalAction.Right    -> $"Drag onto a {Text.orSymbol} goal to choose its right symbol"
        | GoalAction.Cases    -> $"Drag onto a {Text.andSymbol} goal to create separate cases"
        | GoalAction.Expand   -> $"Drag onto a {Text.notSymbol} goal to expand it"

/// Term actions available to the user. These have a mapping
/// to tactics that is not 1:1.
[<RequireQualifiedAccess>]
type TermAction =

    /// Eliminates a goal.
    | Exact

    /// Dissolves a term.
    | Dissolve

    /// Applies a function.
    | Apply

    /// Splits a case into multiple sub-cases.
    | Cases

    /// Expands a negation.
    | Expand

module TermAction =

    let emoji = function
        | TermAction.Exact    -> "❤️"
        | TermAction.Dissolve -> "🦋"
        | TermAction.Apply    -> "👣"
        | TermAction.Cases    -> "👊🏾"
        | TermAction.Expand   -> "🧣"

    let instructions = function
        | TermAction.Exact    -> "Drag onto a symbol that matches the goal"
        | TermAction.Dissolve -> $"Drag onto a {Text.andSymbol} symbol to free it"
        | TermAction.Apply    -> "Drag onto ▢→■ when the goal is ■ to change the goal to ▢"
        | TermAction.Cases    -> $"Drag onto a {Text.orSymbol} term to create separate cases"
        | TermAction.Expand   -> $"Drag onto a {Text.notSymbol} term to expand it"

/// A puzzle to be solved.
type Level =
    {
        /// Proposition to be proved.
        Goal : Type

        /// Hypotheses.
        Terms : Set<Term>

        /// Available goal-level actions.
        GoalActions : Set<GoalAction>

        /// Available term-level actions.
        TermActions : Set<TermAction>

        /// Hint for the user.
        Instructions : string
    }

module Level =

    let private p = Primitive "P"
    let private q = Primitive "Q"
    let private r = Primitive "R"

    let private pq = Function (p, q)
    let private qr = Function (q, r)
    let private pr = Function (p, r)
    let private pqr = Function (p, Function (q, r))

    let private p_and_q = Product [p; q]
    let private p_or_q = Sum [p; q]

    let private exact      = TermAction.emoji TermAction.Exact
    let private intro      = GoalAction.emoji GoalAction.Intro
    let private apply      = TermAction.emoji TermAction.Apply
    let private casesGoal  = GoalAction.emoji GoalAction.Cases
    let private casesTerm  = TermAction.emoji TermAction.Cases
    let private left       = GoalAction.emoji GoalAction.Left
    let private right      = GoalAction.emoji GoalAction.Right
    let private dissolve   = TermAction.emoji TermAction.Dissolve
    let private expandGoal = GoalAction.emoji GoalAction.Expand
    let private expandTerm = TermAction.emoji TermAction.Expand

    /// Builds terms from types.
    let private terms types =
        types
            |> Seq.map Term.create
            |> set

    module private Exact =

        let private goalActions = set []
        let private termActions = set [ TermAction.Exact ]

        /// Introduces the "exact" action.
        let level1 =
            {
                Goal = p
                Terms = terms [p; q]
                GoalActions = goalActions
                TermActions = termActions
                Instructions =
                    $"Drag {exact} onto the symbol that matches the top goal"
            }

        /// More practice with "exact".
        let level2 =
            {
                Goal = r
                Terms = terms [p; q; r]
                GoalActions = goalActions
                TermActions = termActions
                Instructions = ""
            }

        /// Introduces function types.
        let level3 =
            {
                Goal = pq
                Terms = terms [p; q; pq]
                GoalActions = goalActions
                TermActions = termActions
                Instructions = $"You can also use {exact} on more complex symbols"
            }

    module private Intro =

        let private goalActions = set [ GoalAction.Intro ]
        let private termActions = set [ TermAction.Exact ]

        /// Introduces the "intro" action with Q ⊢ P → Q.
        let level1 =
            {
                Goal = pq
                Terms = terms [q]
                GoalActions = goalActions
                TermActions = termActions
                Instructions = $"Drag {intro} onto an arrow goal to simplify it"
            }

        /// P → P.
        let level2 =
            {
                Goal = Function (p, p)
                Terms = terms []
                GoalActions = goalActions
                TermActions = termActions
                Instructions = ""
            }

        /// More practice with intro.
        let level3 =
            {
                Goal = pqr
                Terms = terms [r]
                GoalActions = goalActions
                TermActions = termActions
                Instructions = ""
            }

    module private LeftRight =

        let private goalActions =
            set [
                GoalAction.Left
                GoalAction.Right
            ]
        let private termActions = set [ TermAction.Exact ]

        /// Introduces the left action.
        let level1 =
            {
                Goal = Sum [p; q]
                Terms = terms [p]
                GoalActions = goalActions
                TermActions = termActions
                Instructions = $"Drag {left} or {right} onto a {Text.orSymbol} goal to simplify it"
            }

        /// Introduces the right action.
        let level2 =
            {
                Goal = Sum [p; qr]
                Terms = terms [qr]
                GoalActions = goalActions
                TermActions = termActions
                Instructions = ""
            }

    module private Dissolve =

        /// Introduces the dissolve action.
        let level1 =
            {
                Goal = p
                Terms = terms [ p_and_q ]
                GoalActions = set []
                TermActions =
                    set [
                        TermAction.Exact
                        TermAction.Dissolve
                    ]
                Instructions = $"Drag {dissolve} onto a {Text.andSymbol} symbol to dissolve it"
            }

    module private Apply =

        /// Modus ponens.
        let level1 =
            {
                Goal = q
                Terms = terms [p; pq]
                GoalActions = set []
                TermActions =
                    set [
                        TermAction.Exact
                        TermAction.Apply
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
                GoalActions = set [ GoalAction.Intro ]
                TermActions =
                    set [
                        TermAction.Exact
                        TermAction.Apply
                    ]
                Instructions = ""
            }

        /// Currying.
        let level3 =
            {
                Goal = Function (p_and_q, r)
                Terms = terms [pqr]
                GoalActions = set [ GoalAction.Intro ]
                TermActions =
                    set [
                        TermAction.Exact
                        TermAction.Apply
                    ]
                Instructions = $"You can also use {apply} on nested ▢→■ symbols when the goal is ■"
            }

    module private Cases =

        /// Commutivity of ∨.
        let level1 =
            {
                Goal = Sum [q; p]
                Terms = terms [p_or_q]
                GoalActions =
                    set [
                        GoalAction.Left
                        GoalAction.Right
                    ]
                TermActions =
                    set [
                        TermAction.Exact
                        TermAction.Apply
                        TermAction.Cases
                    ]
                Instructions = $"Drag {casesTerm} onto {Text.orSymbol} in the field to create separate cases"
            }

        /// More practice with multiple cases.
        let level2 =
            {
                Goal = r
                Terms =
                    terms [
                        p_or_q
                        pr
                        qr
                    ]
                GoalActions = set []
                TermActions =
                    set [
                        TermAction.Exact
                        TermAction.Apply
                        TermAction.Cases
                    ]
                Instructions = ""
            }

        /// Commutivity of ∧.
        let level3 =
            {
                Goal = Product [q; p]
                Terms = terms [p_and_q]
                GoalActions = set [ GoalAction.Cases ]
                TermActions =
                    set [
                        TermAction.Exact
                        TermAction.Dissolve
                    ]
                Instructions = $"You can drag {casesGoal} onto a {Text.andSymbol} goal to create separate cases"
            }

        /// Exportation.
        let level4 =
            {
                Goal = pqr
                Terms = terms [ Function (p_and_q, r) ]
                GoalActions =
                    set [
                        GoalAction.Intro
                        GoalAction.Cases
                    ]
                TermActions =
                    set [
                        TermAction.Exact
                        TermAction.Apply
                    ]
                Instructions = ""
            }

        /// Distributive property.
        let level5 =
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
                GoalActions =
                    set [
                        GoalAction.Intro
                        GoalAction.Left
                        GoalAction.Right
                        GoalAction.Cases
                    ]
                TermActions =
                    set [
                        TermAction.Exact
                        TermAction.Apply
                        TermAction.Dissolve
                        TermAction.Cases
                    ]
                Instructions = ""
            }

    module private Negation =

        /// Double negative (but not the law of excluded middle).
        let level1 =
            {
                Goal = Type.not (Type.not p)
                Terms = terms [ p ]
                GoalActions =
                    set [
                        GoalAction.Intro
                        GoalAction.Expand
                    ]
                TermActions =
                    set [
                        TermAction.Exact
                        TermAction.Apply
                    ]
                Instructions = $"Drag onto a {Text.notSymbol} to expand it"
            }

        /// Modus tollens.
        let level2 =
            {
                Goal = Function (Type.not q, Type.not p)
                Terms = terms [ pq ]
                GoalActions =
                    set [
                        GoalAction.Intro
                        GoalAction.Expand
                    ]
                TermActions =
                    set [
                        TermAction.Exact
                        TermAction.Apply
                        TermAction.Expand
                    ]
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
                GoalActions =
                    set [
                        GoalAction.Intro
                        GoalAction.Expand
                    ]
                TermActions =
                    set [
                        TermAction.Exact
                        TermAction.Dissolve
                        TermAction.Apply
                        TermAction.Cases
                        TermAction.Expand
                    ]
                Instructions = ""
            }

        /// de Morgan's laws.
        let level4 =
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
                GoalActions =
                    set [
                        GoalAction.Intro
                        GoalAction.Left
                        GoalAction.Right
                        GoalAction.Cases
                        GoalAction.Expand
                    ]
                TermActions =
                    set [
                        TermAction.Exact
                        TermAction.Dissolve
                        TermAction.Apply
                        TermAction.Cases
                        TermAction.Expand
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

            LeftRight.level1
            LeftRight.level2

            Dissolve.level1

            Apply.level1
            Apply.level2
            Apply.level3

            Cases.level1
            Cases.level2
            Cases.level3
            Cases.level4
            Cases.level5

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
