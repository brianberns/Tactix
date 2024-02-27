namespace Tactix

module Text =

    let andSymbol = "🍓"
    let orSymbol  = "🍒"
    let notSymbol = "☂️"
    let implies   = "👉🏾"

// https://stackoverflow.com/questions/64929689/avoiding-the-error-where-a-module-and-a-type-definition-occur-in-two-parts-of-an
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TacticType =

    let emoji = function
        | TacticType.Intro        -> "🚀"
        | TacticType.Split        -> "💥"
        | TacticType.Exact        -> "❤️"
        | TacticType.DissolveGoal -> "🦋"
        | TacticType.DissolveTerm -> "🦋"
        | TacticType.Apply        -> "👣"
        | TacticType.Cases        -> "💥"
        | TacticType.AffirmGoal   -> "🌈"
        | TacticType.AffirmTerm   -> "🌈"

    let instructions = function
        | TacticType.Intro        -> $"Drag onto a {Text.implies} goal to simplify it"
        | TacticType.Split        -> $"Drag onto a {Text.andSymbol} goal to create separate cases"
        | TacticType.Exact        -> "Drag onto a symbol that matches the goal"
        | TacticType.DissolveGoal -> $"Drag onto a {Text.orSymbol} goal to simplify it"
        | TacticType.DissolveTerm -> $"Drag onto a {Text.andSymbol} symbol from below to simplify it"
        | TacticType.Apply        -> $"Drag onto ▢{Text.implies}■ when the goal is ■ to change the goal to ▢"
        | TacticType.Cases        -> $"Drag onto a {Text.orSymbol} symbol from below to create separate cases"
        | TacticType.AffirmGoal   -> $"Drag onto a {Text.notSymbol} goal to remove {Text.notSymbol}"
        | TacticType.AffirmTerm   -> $"Drag onto a {Text.notSymbol} symbol from below to remove {Text.notSymbol}"

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

    let private exact        = TacticType.emoji TacticType.Exact
    let private intro        = TacticType.emoji TacticType.Intro
    let private apply        = TacticType.emoji TacticType.Apply
    let private casesGoal    = TacticType.emoji TacticType.Split
    let private casesTerm    = TacticType.emoji TacticType.Cases
    let private dissolveGoal = TacticType.emoji TacticType.DissolveGoal
    let private dissolveTerm = TacticType.emoji TacticType.DissolveTerm
    let private affirmGoal   = TacticType.emoji TacticType.AffirmGoal
    let private affirmTerm   = TacticType.emoji TacticType.AffirmTerm

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
                Instructions = $"Drag {intro} onto a {Text.implies} goal to simplify it"
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

    module private Dissolve =

        /// Introduces the dissolve goal tactic.
        let level1 =
            {
                Goal = Sum [p; q]
                Terms = terms [p]
                GoalTactics = set [ TacticType.DissolveGoal ]
                TermTactics = set [ TacticType.Exact ]
                Instructions = $"Drag {dissolveGoal} onto a {Text.orSymbol} goal to simplify it"
            }

        /// Introduces the dissolve term tactic.
        let level2 =
            {
                Goal = p
                Terms = terms [ p_and_q ]
                GoalTactics = set []
                TermTactics =
                    set [
                        TacticType.Exact
                        TacticType.DissolveTerm
                    ]
                Instructions = $"Drag {dissolveTerm} onto a {Text.andSymbol} symbol to simplify it"
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

        /// Commutivity of ∧.
        let level1 =
            {
                Goal = Product [q; p]
                Terms = terms [p_and_q]
                GoalTactics = set [ TacticType.Split ]
                TermTactics =
                    set [
                        TacticType.Exact
                        TacticType.DissolveTerm
                    ]
                Instructions = $"You can drag {casesGoal} onto a {Text.andSymbol} goal to create separate cases"
            }

        /// Commutivity of ∨.
        let level2 =
            {
                Goal = Sum [q; p]
                Terms = terms [p_or_q]
                GoalTactics = set [ TacticType.DissolveGoal ]
                TermTactics =
                    set [
                        TacticType.Exact
                        TacticType.Cases
                    ]
                Instructions = $"Drag {casesTerm} onto a {Text.orSymbol} symbol to create separate cases"
            }

        /// More practice with multiple cases.
        let level3 =
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
                        TacticType.DissolveGoal
                        TacticType.Split
                    ]
                TermTactics =
                    set [
                        TacticType.Exact
                        TacticType.Apply
                        TacticType.DissolveTerm
                        TacticType.Cases
                    ]
                Instructions = ""
            }

    module private Negation =

        /// Double negative (but not the law of excluded middle).
        let level1 =
            {
                Goal = Not (Not p)
                Terms = terms [p]
                GoalTactics =
                    set [
                        TacticType.Intro
                        TacticType.AffirmGoal
                    ]
                TermTactics =
                    set [
                        TacticType.Exact
                        TacticType.Apply
                        TacticType.AffirmTerm
                    ]
                Instructions = $"Drag {affirmGoal} onto a {Text.notSymbol} to remove it"
            }

        /// Modus tollens.
        let level2 =
            {
                Goal = Function (Not q, Not p)
                Terms = terms [ pq ]
                GoalTactics =
                    set [
                        TacticType.Intro
                        TacticType.AffirmGoal
                    ]
                TermTactics =
                    set [
                        TacticType.Exact
                        TacticType.Apply
                        TacticType.AffirmTerm
                    ]
                Instructions = ""
            }

        /// de Morgan's laws.
        let level3 =
            {
                Goal = Not p_and_q
                Terms =
                    terms [
                        Sum [
                            Not p
                            Not q
                        ]
                    ]
                GoalTactics =
                    set [
                        TacticType.Intro
                        TacticType.AffirmGoal
                    ]
                TermTactics =
                    set [
                        TacticType.Exact
                        TacticType.DissolveTerm
                        TacticType.Apply
                        TacticType.Cases
                        TacticType.AffirmTerm
                    ]
                Instructions = ""
            }

        /// de Morgan's laws.
        let level4 =
            {
                Goal =
                    Product [
                        Not p
                        Not q
                    ]
                Terms =
                    terms [
                        Not (Sum [p; q])
                    ]
                GoalTactics =
                    set [
                        TacticType.Intro
                        TacticType.DissolveGoal
                        TacticType.Split
                        TacticType.AffirmGoal
                    ]
                TermTactics =
                    set [
                        TacticType.Exact
                        TacticType.DissolveTerm
                        TacticType.Apply
                        TacticType.Cases
                        TacticType.AffirmTerm
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

            Dissolve.level1
            Dissolve.level2

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

    /// Starts a proof for the given level.
    let initializeProof level =
        let case =
            {
                Goals = Set.singleton level.Goal
                Terms = level.Terms
                IsComplete = false
            }
        Proof.empty
            |> Proof.add None case
