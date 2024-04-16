namespace Tactix

module Text =

    let andSymbol = "🌳"
    let orSymbol  = "🎀"
    let notSymbol = "☂️"
    let implies   = "👉🏾"

// https://stackoverflow.com/questions/64929689/avoiding-the-error-where-a-module-and-a-type-definition-occur-in-two-parts-of-an
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TacticType =

    let emoji = function
        | TacticType.Intro        -> "🚀"
        | TacticType.Exact        -> "❤️"
        | TacticType.Apply        -> "👣"
        | TacticType.DissolveGoal -> "✂️"
        | TacticType.DissolveTerm -> "🪚"
        | TacticType.SplitGoal    -> "🍂"
        | TacticType.SplitTerm    -> "💥"
        | TacticType.AffirmGoal   -> "🌈"
        | TacticType.AffirmTerm   -> "🌈"

    let intro        = emoji TacticType.Intro
    let exact        = emoji TacticType.Exact
    let apply        = emoji TacticType.Apply
    let dissolveGoal = emoji TacticType.DissolveGoal
    let dissolveTerm = emoji TacticType.DissolveTerm
    let splitGoal    = emoji TacticType.SplitGoal
    let splitTerm    = emoji TacticType.SplitTerm
    let affirmGoal   = emoji TacticType.AffirmGoal
    let affirmTerm   = emoji TacticType.AffirmTerm

    let instruction = function
        | TacticType.Intro        -> $"Drag {intro} onto a {Text.implies} goal to simplify it"
        | TacticType.Exact        -> $"Drag {exact} onto a symbol that matches the goal"
        | TacticType.Apply        -> $"Drag {apply} onto ▢{Text.implies}■ when the goal is ■ to change the goal to ▢"
        | TacticType.DissolveGoal -> $"Drag {dissolveGoal} onto a {Text.orSymbol} goal to simplify it"
        | TacticType.DissolveTerm -> $"Drag {dissolveTerm} onto a {Text.andSymbol} symbol to simplify it"
        | TacticType.SplitGoal    -> $"Drag {splitGoal} onto a {Text.andSymbol} goal to create separate cases"
        | TacticType.SplitTerm    -> $"Drag {splitTerm} onto a {Text.orSymbol} symbol to create separate cases"
        | TacticType.AffirmGoal   -> $"Drag {affirmGoal} onto a {Text.notSymbol} goal to remove {Text.notSymbol}"
        | TacticType.AffirmTerm   -> $"Drag {affirmTerm} onto a {Text.notSymbol} symbol to remove {Text.notSymbol}"

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
        Instruction : string
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

    /// Builds terms from types.
    let private terms types =
        types
            |> Seq.map Term.create
            |> set

    module private Exact =

        let goalTactics = set []
        let termTactics = set [ TacticType.Exact ]

        /// Introduces the "exact" tactic.
        let level1 =
            {
                Goal = p
                Terms = terms [p; q]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instruction =
                    $"Drag {TacticType.exact} onto the symbol that matches the top goal"
            }

        /// More practice with "exact".
        let level2 =
            {
                Goal = r
                Terms = terms [p; q; r]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instruction = ""
            }

        /// Introduces function types.
        let level3 =
            {
                Goal = pq
                Terms = terms [p; q; pq]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instruction = $"You can also use {TacticType.exact} on more complex symbols"
            }

    module private Intro =

        let goalTactics =
            Exact.goalTactics + set [ TacticType.Intro ]
        let termTactics = Exact.termTactics

        /// Introduces the "intro" tactic with Q ⊢ P → Q.
        let level1 =
            {
                Goal = pq
                Terms = terms [q]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instruction = $"Drag {TacticType.intro} onto a {Text.implies} goal to simplify it"
            }

        /// P → P.
        let level2 =
            {
                Goal = Function (p, p)
                Terms = terms []
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instruction = ""
            }

        /// More practice with intro.
        let level3 =
            {
                Goal = pqr
                Terms = terms [r]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instruction = ""
            }

    module private Dissolve =

        let goalTactics =
            Intro.goalTactics + set [ TacticType.DissolveGoal ]
        let termTactics =
            Intro.termTactics + set [ TacticType.DissolveTerm ]

        /// Introduces the dissolve goal tactic.
        let level1 =
            {
                Goal = Sum [p; q]
                Terms = terms [p]
                GoalTactics = goalTactics
                TermTactics = Intro.termTactics
                Instruction = $"Drag {TacticType.dissolveGoal} onto a {Text.orSymbol} goal to simplify it"
            }

        /// Introduces the dissolve term tactic.
        let level2 =
            {
                Goal = p
                Terms = terms [ p_and_q ]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instruction = $"Drag {TacticType.dissolveTerm} onto a {Text.andSymbol} symbol to simplify it"
            }

    module private Apply =

        let goalTactics = Dissolve.goalTactics
        let termTactics =
            Dissolve.termTactics + set [ TacticType.Apply ]

        /// Modus ponens.
        let level1 =
            {
                Goal = q
                Terms = terms [p; pq]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instruction = $"Drag {TacticType.apply} onto ▢{Text.implies}■ when the goal is ■ to change the goal to ▢"
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
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instruction = ""
            }

        /// Currying.
        let level3 =
            {
                Goal = Function (p_and_q, r)
                Terms = terms [pqr]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instruction = $"You can also use {TacticType.apply} on nested ▢{Text.implies}■ symbols when the goal is ■"
            }

    module private Split =

        let goalTactics =
            Apply.goalTactics + set [ TacticType.SplitGoal ]
        let termTactics =
            Apply.termTactics + set [ TacticType.SplitTerm ]

        /// Commutivity of ∧.
        let level1 =
            {
                Goal = Product [q; p]
                Terms = terms [p_and_q]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instruction = $"Drag {TacticType.splitGoal} onto a {Text.andSymbol} goal to create separate cases"
            }

        /// Commutivity of ∨.
        let level2 =
            {
                Goal = Sum [q; p]
                Terms = terms [p_or_q]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instruction = $"Drag {TacticType.splitTerm} onto a {Text.orSymbol} symbol to create separate cases"
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
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instruction = ""
            }

        /// Exportation.
        let level4 =
            {
                Goal = pqr
                Terms = terms [ Function (p_and_q, r) ]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instruction = ""
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
                        TacticType.SplitGoal
                    ]
                TermTactics =
                    set [
                        TacticType.Exact
                        TacticType.Apply
                        TacticType.DissolveTerm
                        TacticType.SplitTerm
                    ]
                Instruction = ""
            }

    module private Negation =

        let goalTactics =
            Split.goalTactics + set [ TacticType.AffirmGoal ]
        let termTactics =
            Split.termTactics + set [ TacticType.AffirmTerm ]

        /// Double negative (but not the law of excluded middle).
        let level1 =
            {
                Goal = Not (Not p)
                Terms = terms [p]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instruction = $"Drag {TacticType.affirmGoal} onto a {Text.notSymbol} to remove it"
            }

        /// Modus tollens.
        let level2 =
            {
                Goal = Function (Not q, Not p)
                Terms = terms [ pq ]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instruction = ""
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
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instruction = ""
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
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instruction = ""
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

            Split.level1
            Split.level2
            Split.level3
            Split.level4
            Split.level5

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
