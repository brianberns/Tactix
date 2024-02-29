namespace Tactix

module Text =

    let andSymbol = "🐛"
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
        | TacticType.DissolveGoal -> "🎈"
        | TacticType.DissolveTerm -> "🦋"
        | TacticType.SplitGoal    -> "🐉"
        | TacticType.SplitTerm    -> "💥"
        | TacticType.AffirmGoal   -> "🌈"
        | TacticType.AffirmTerm   -> "🌈"
        | TacticType.AddZero      -> "+0"
        | TacticType.AddSucc      -> "+S"

    let instructions = function
        | TacticType.Intro        -> $"Drag onto a {Text.implies} goal to simplify it"
        | TacticType.Exact        -> "Drag onto a symbol that matches the goal"
        | TacticType.Apply        -> $"Drag onto ▢{Text.implies}■ when the goal is ■ to change the goal to ▢"
        | TacticType.DissolveGoal -> $"Drag onto a {Text.orSymbol} goal to simplify it"
        | TacticType.DissolveTerm -> $"Drag onto a {Text.andSymbol} symbol to simplify it"
        | TacticType.SplitGoal    -> $"Drag onto a {Text.andSymbol} goal to create separate cases"
        | TacticType.SplitTerm    -> $"Drag onto a {Text.orSymbol} symbol to create separate cases"
        | TacticType.AffirmGoal   -> $"Drag onto a {Text.notSymbol} goal to remove {Text.notSymbol}"
        | TacticType.AffirmTerm   -> $"Drag onto a {Text.notSymbol} symbol to remove {Text.notSymbol}"
        | TacticType.AddZero      -> "Add zero"
        | TacticType.AddSucc      -> "Add succ"

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
    let private splitGoal    = TacticType.emoji TacticType.SplitGoal
    let private splitTerm    = TacticType.emoji TacticType.SplitTerm
    let private dissolveGoal = TacticType.emoji TacticType.DissolveGoal
    let private dissolveTerm = TacticType.emoji TacticType.DissolveTerm
    let private affirmGoal   = TacticType.emoji TacticType.AffirmGoal
    let private affirmTerm   = TacticType.emoji TacticType.AffirmTerm

    let private p = Primitive "P"
    let private q = Primitive "Q"
    let private r = Primitive "R"

    let private pq = Impliction (p, q)
    let private qr = Impliction (q, r)
    let private pr = Impliction (p, r)
    let private pqr = Impliction (p, Impliction (q, r))

    let private p_and_q = Conjunction [p; q]
    let private p_or_q = Disjunction [p; q]

    /// Builds terms from propositions.
    let private terms props =
        props
            |> Seq.map (Proposition >> Term.create)
            |> set

    module private Exact =

        let goalTactics = set []
        let termTactics = set [ TacticType.Exact ]

        /// Introduces the "exact" tactic.
        let level1 =
            {
                Goal = Proposition p
                Terms = terms [p; q]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions =
                    $"Drag {exact} onto the symbol that matches the top goal"
            }

        /// More practice with "exact".
        let level2 =
            {
                Goal = Proposition r
                Terms = terms [p; q; r]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = ""
            }

        /// Introduces function types.
        let level3 =
            {
                Goal = Proposition pq
                Terms = terms [p; q; pq]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = $"You can also use {exact} on more complex symbols"
            }

    module private Intro =

        let goalTactics =
            Exact.goalTactics + set [ TacticType.Intro ]
        let termTactics = Exact.termTactics

        /// Introduces the "intro" tactic with Q ⊢ P → Q.
        let level1 =
            {
                Goal = Proposition pq
                Terms = terms [q]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = $"Drag {intro} onto a {Text.implies} goal to simplify it"
            }

        /// P → P.
        let level2 =
            {
                Goal = Proposition (Impliction (p, p))
                Terms = terms []
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = ""
            }

        /// More practice with intro.
        let level3 =
            {
                Goal = Proposition pqr
                Terms = terms [r]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = ""
            }

    module private Dissolve =

        let goalTactics =
            Intro.goalTactics + set [ TacticType.DissolveGoal ]
        let termTactics =
            Intro.termTactics + set [ TacticType.DissolveTerm ]

        /// Introduces the dissolve goal tactic.
        let level1 =
            {
                Goal = Proposition (Disjunction [p; q])
                Terms = terms [p]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = $"Drag {dissolveGoal} onto a {Text.orSymbol} goal to simplify it"
            }

        /// Introduces the dissolve term tactic.
        let level2 =
            {
                Goal = Proposition p
                Terms = terms [ p_and_q ]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = $"Drag {dissolveTerm} onto a {Text.andSymbol} symbol to simplify it"
            }

    module private Apply =

        let goalTactics = Dissolve.goalTactics
        let termTactics =
            Dissolve.termTactics + set [ TacticType.Apply ]

        /// Modus ponens.
        let level1 =
            {
                Goal = Proposition q
                Terms = terms [p; pq]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = $"Drag {apply} onto ▢{Text.implies}■ when the goal is ■ to change the goal to ▢"
            }

        /// Implication is transitive.
        let level2 =
            {
                Goal = Proposition (Impliction (p, r))
                Terms =
                    terms [
                        Impliction (p, q)
                        Impliction (q, r)
                    ]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = ""
            }

        /// Currying.
        let level3 =
            {
                Goal = Proposition (Impliction (p_and_q, r))
                Terms = terms [pqr]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = $"You can also use {apply} on nested ▢{Text.implies}■ symbols when the goal is ■"
            }

    module private Split =

        let goalTactics =
            Apply.goalTactics + set [ TacticType.SplitGoal ]
        let termTactics =
            Apply.termTactics + set [ TacticType.SplitTerm ]

        /// Commutivity of ∧.
        let level1 =
            {
                Goal = Proposition (Conjunction [q; p])
                Terms = terms [p_and_q]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = $"Drag {splitGoal} onto a {Text.andSymbol} goal to create separate cases"
            }

        /// Commutivity of ∨.
        let level2 =
            {
                Goal = Proposition (Disjunction [q; p])
                Terms = terms [p_or_q]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = $"Drag {splitTerm} onto a {Text.orSymbol} symbol to create separate cases"
            }

        /// More practice with multiple cases.
        let level3 =
            {
                Goal = Proposition r
                Terms =
                    terms [
                        p_or_q
                        pr
                        qr
                    ]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = ""
            }

        /// Exportation.
        let level4 =
            {
                Goal = Proposition pqr
                Terms = terms [ Impliction (p_and_q, r) ]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = ""
            }

        /// Distributive property.
        let level5 =
            {
                Goal =
                    Proposition (Disjunction [
                        Conjunction [p; r]
                        Conjunction [q; r]
                    ])
                Terms =
                    terms [
                        Conjunction [Disjunction [p; q]; r]
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
                Instructions = ""
            }

    module private Negation =

        let goalTactics =
            Split.goalTactics + set [ TacticType.AffirmGoal ]
        let termTactics =
            Split.termTactics + set [ TacticType.AffirmTerm ]

        /// Double negative (but not the law of excluded middle).
        let level1 =
            {
                Goal = Proposition (Not (Not p))
                Terms = terms [p]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = $"Drag {affirmGoal} onto a {Text.notSymbol} to remove it"
            }

        /// Modus tollens.
        let level2 =
            {
                Goal = Proposition (Impliction (Not q, Not p))
                Terms = terms [ pq ]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = ""
            }

        /// de Morgan's laws.
        let level3 =
            {
                Goal = Proposition (Not p_and_q)
                Terms =
                    terms [
                        Disjunction [
                            Not p
                            Not q
                        ]
                    ]
                GoalTactics = goalTactics
                TermTactics = termTactics
                Instructions = ""
            }

        /// de Morgan's laws.
        let level4 =
            {
                Goal =
                    Proposition (Conjunction [
                        Not p
                        Not q
                    ])
                Terms =
                    terms [
                        Not (Disjunction [p; q])
                    ]
                GoalTactics = goalTactics
                TermTactics = termTactics
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
