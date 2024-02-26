namespace Tactix

module Text =

    let andSymbol = "🐛"
    let orSymbol = "👈🏾👉🏾"
    let notSymbol = "⛔"
    let implies = "➡️"

    let andHtml = andSymbol
    let orHtml = "👈🏾<br />👉🏾"
    let notHtml = notSymbol
    let impliesHtml = implies

// https://stackoverflow.com/questions/64929689/avoiding-the-error-where-a-module-and-a-type-definition-occur-in-two-parts-of-an
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TacticType =

    let emoji = function
        | TacticType.Intro    -> "🚀"
        | TacticType.Left     -> "👈🏾"
        | TacticType.Right    -> "👉🏾"
        | TacticType.Split    -> "🐞"
        | TacticType.Exact    -> "❤️"
        | TacticType.Dissolve -> "🦋"
        | TacticType.Apply    -> "👣"
        | TacticType.Cases    -> "👊🏾"

    let instructions = function
        | TacticType.Intro    -> "Drag onto an arrow goal to simplify it"
        | TacticType.Left     -> $"Drag onto a {Text.orSymbol} goal to choose its left symbol"
        | TacticType.Right    -> $"Drag onto a {Text.orSymbol} goal to choose its right symbol"
        | TacticType.Split    -> $"Drag onto a {Text.andSymbol} goal to create separate cases"
        | TacticType.Exact    -> "Drag onto a symbol that matches the goal"
        | TacticType.Dissolve -> $"Drag onto a {Text.andSymbol} symbol to free it"
        | TacticType.Apply    -> $"Drag onto ▢{Text.implies}■ when the goal is ■ to change the goal to ▢"
        | TacticType.Cases    -> $"Drag onto a given {Text.orSymbol} to create separate cases"

/// A puzzle to be solved.
type Level =
    {
        /// Proposition to be proved.
        Goal : Type

        /// Hypotheses.
        Terms : Set<Term>

        /// Available goal-level tactics.
        GoalTactics : Set<TacticType>

        /// Available term-level tactics.
        TermTactics : Set<TacticType>

        /// Hint for the user.
        Instructions : string
    }

module Level =

    let private exact      = TacticType.emoji TacticType.Exact
    let private intro      = TacticType.emoji TacticType.Intro
    let private apply      = TacticType.emoji TacticType.Apply
    let private casesGoal  = TacticType.emoji TacticType.Split
    let private casesTerm  = TacticType.emoji TacticType.Cases
    let private left       = TacticType.emoji TacticType.Left
    let private right      = TacticType.emoji TacticType.Right
    let private dissolve   = TacticType.emoji TacticType.Dissolve

    let private p = Primitive "P"
    let private q = Primitive "Q"
    let private r = Primitive "R"

    let private pq = Function (p, q)
    let private qr = Function (q, r)
    let private pr = Function (p, r)
    let private pqr = Function (p, Function (q, r))

    let private p_and_q = Product [p; q]
    let private p_or_q = Sum [p; q]

    /// Builds terms from types.
    let private terms types =
        types
            |> Seq.map Term.create
            |> set

    module private Exact =

        let private goalTactics = set []
        let private termTactics = set [ TacticType.Exact ]

        /// Introduces the "exact" tactic.
        let level1 =
            {
                Goal = p
                Terms = terms [p; q]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions =
                    $"Drag {exact} onto the symbol that matches the top goal"
            }

        /// More practice with "exact".
        let level2 =
            {
                Goal = r
                Terms = terms [p; q; r]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = ""
            }

        /// Introduces function types.
        let level3 =
            {
                Goal = pq
                Terms = terms [p; q; pq]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = $"You can also use {exact} on more complex symbols"
            }

    module private Intro =

        let private goalTactics = set [ TacticType.Intro ]
        let private termTactics = set [ TacticType.Exact ]

        /// Introduces the "intro" tactic with Q ⊢ P → Q.
        let level1 =
            {
                Goal = pq
                Terms = terms [q]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = $"Drag {intro} onto an arrow goal to simplify it"
            }

        /// P → P.
        let level2 =
            {
                Goal = Function (p, p)
                Terms = terms []
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = ""
            }

        /// More practice with intro.
        let level3 =
            {
                Goal = pqr
                Terms = terms [r]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = ""
            }

    module private LeftRight =

        let private goalTactics =
            set [
                TacticType.Left
                TacticType.Right
            ]
        let private termTactics = set [ TacticType.Exact ]

        /// Introduces the left tactic.
        let level1 =
            {
                Goal = Sum [p; q]
                Terms = terms [p]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = $"Drag {left} or {right} onto a {Text.orSymbol} goal to simplify it"
            }

        /// Introduces the right tactic.
        let level2 =
            {
                Goal = Sum [p; qr]
                Terms = terms [qr]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = ""
            }

    module private Dissolve =

        /// Introduces the dissolve tactic.
        let level1 =
            {
                Goal = p
                Terms = terms [ p_and_q ]
                GoalTactics = set []
                TermTactics =
                    set [
                        TacticType.Exact
                        TacticType.Dissolve
                    ]
                Instructions = $"Drag {dissolve} onto a {Text.andSymbol} symbol to dissolve it"
            }

    module private Apply =

        /// Modus ponens.
        let level1 =
            {
                Goal = q
                Terms = terms [p; pq]
                GoalTactics = set []
                TermTactics =
                    set [
                        TacticType.Exact
                        TacticType.Apply
                    ]
                Instructions = $"Drag {apply} onto ▢{Text.implies}■ when the goal is ■ to change the goal to ▢"
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
                GoalTactics = set [ TacticType.Intro ]
                TermTactics =
                    set [
                        TacticType.Exact
                        TacticType.Apply
                    ]
                Instructions = ""
            }

        /// Currying.
        let level3 =
            {
                Goal = Function (p_and_q, r)
                Terms = terms [pqr]
                GoalTactics = set [ TacticType.Intro ]
                TermTactics =
                    set [
                        TacticType.Exact
                        TacticType.Apply
                    ]
                Instructions = $"You can also use {apply} on nested ▢{Text.implies}■ symbols when the goal is ■"
            }

    module private Cases =

        /// Commutivity of ∨.
        let level1 =
            {
                Goal = Sum [q; p]
                Terms = terms [p_or_q]
                GoalTactics =
                    set [
                        TacticType.Left
                        TacticType.Right
                    ]
                TermTactics =
                    set [
                        TacticType.Exact
                        TacticType.Cases
                    ]
                Instructions = $"Drag {casesTerm} onto a given {Text.orSymbol} to create separate cases"
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
                GoalTactics = set []
                TermTactics =
                    set [
                        TacticType.Exact
                        TacticType.Apply
                        TacticType.Cases
                    ]
                Instructions = ""
            }

        /// Commutivity of ∧.
        let level3 =
            {
                Goal = Product [q; p]
                Terms = terms [p_and_q]
                GoalTactics = set [ TacticType.Split ]
                TermTactics =
                    set [
                        TacticType.Exact
                        TacticType.Dissolve
                    ]
                Instructions = $"You can drag {casesGoal} onto a {Text.andSymbol} goal to create separate cases"
            }

        /// Exportation.
        let level4 =
            {
                Goal = pqr
                Terms = terms [ Function (p_and_q, r) ]
                GoalTactics =
                    set [
                        TacticType.Intro
                        TacticType.Split
                    ]
                TermTactics =
                    set [
                        TacticType.Exact
                        TacticType.Apply
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
                GoalTactics =
                    set [
                        TacticType.Intro
                        TacticType.Left
                        TacticType.Right
                        TacticType.Split
                    ]
                TermTactics =
                    set [
                        TacticType.Exact
                        TacticType.Apply
                        TacticType.Dissolve
                        TacticType.Cases
                    ]
                Instructions = ""
            }

    (*
    module private Negation =

        /// Double negative (but not the law of excluded middle).
        let level1 =
            {
                Goal = Type.not (Type.not p)
                Terms = terms [ p ]
                GoalTactics =
                    set [
                        TacticType.Intro
                        TacticType.Expand
                    ]
                TermTactics =
                    set [
                        TacticType.Exact
                        TacticType.Apply
                    ]
                Instructions = $"Drag {expandGoal} onto a {Text.notSymbol} to expand it"
            }

        /// Modus tollens.
        let level2 =
            {
                Goal = Function (Type.not q, Type.not p)
                Terms = terms [ pq ]
                GoalTactics =
                    set [
                        TacticType.Intro
                        TacticType.Expand
                    ]
                TermTactics =
                    set [
                        TacticType.Exact
                        TacticType.Apply
                        TacticType.Expand
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
                GoalTactics =
                    set [
                        TacticType.Intro
                        TacticType.Expand
                    ]
                TermTactics =
                    set [
                        TacticType.Exact
                        TacticType.Dissolve
                        TacticType.Apply
                        TacticType.Cases
                        TacticType.Expand
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
                GoalTactics =
                    set [
                        TacticType.Intro
                        TacticType.Left
                        TacticType.Right
                        TacticType.Split
                        TacticType.Expand
                    ]
                TermTactics =
                    set [
                        TacticType.Exact
                        TacticType.Dissolve
                        TacticType.Apply
                        TacticType.Cases
                        TacticType.Expand
                    ]
                Instructions = ""
            }
    *)

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

            (*
            Negation.level1
            Negation.level2
            Negation.level3
            Negation.level4
            *)
        |]

    let initializeProof level =
        let case =
            {
                Goals = Set.singleton level.Goal
                Terms = level.Terms
                IsComplete = false
            }
        Proof.empty
            |> Proof.add case
