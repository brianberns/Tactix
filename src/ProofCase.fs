namespace Tactix

/// Tactic types.
[<RequireQualifiedAccess>]
type TacticType =
    | Exact
    | Intro
    | Apply
    | Dissolve
    | Cases
    | Left
    | Right
    | Split

/// A tactic used in a proof. Some of these are from the Lean
/// language.
// https://www.ma.imperial.ac.uk/~buzzard/lean_together/source/contents.html
type Tactic =

    /// Term (HP : P) completes goal P.
    | Exact of Term

    /// Introduces term (HP : P) when goal is P -> Q, changing
    /// goal to just Q.
    | Intro of Term

    /// Applies term (P1 -> P2 -> ... -> PN -> Q), where the
    /// goal is Q, replacing the goal with (P1 ∧ P2 ... ∧ PN).
    /// (This behavior is slightly different from Lean, which
    /// replaces the goal with N separate goals, P1 through PN.)
    | Apply of Term

    /// Dissolves term (HPQ : P ∧ Q) into (HP : P) and (HQ : Q).
    | Dissolve of Term

    /// Breaks up term (HPQ : P ∨ Q) into separate cases for
    /// (HP : P) and (HQ : Q).
    | Cases of Term

    /// Changes goal (P ∨ Q) to just P.
    | Left

    /// Changes goal (P ∨ Q) to just Q.
    | Right

    /// Splits goal (P ∧ Q) into two cases, P and Q.
    | Split

    /// Type of this tactic.
    member tactic.Type =
        match tactic with
            | Exact _    -> TacticType.Exact
            | Intro _    -> TacticType.Intro
            | Apply _    -> TacticType.Apply
            | Dissolve _ -> TacticType.Dissolve
            | Cases _    -> TacticType.Cases
            | Left       -> TacticType.Left
            | Right      -> TacticType.Right
            | Split      -> TacticType.Split

/// One case in a proof.
type ProofCase =
    {
        /// Proposition to be proved.
        GoalOpt : Option<Type>

        /// Given hypotheses.
        Terms : Set<Term>
    }

module ProofCase =

    /// Applies p -> q with the given goal, if possible.
    let private apply p q goal =

        let rec loop p q =
            if q = goal then [p]
            else
                match q with
                    | Function (qIn, qOut) ->
                        let goals = loop qIn qOut
                        if goals = [] then []
                        else p :: goals
                    | _ -> []

        loop p q

    /// Adds the given tactic to the given proof case. If
    /// successful, one or more resulting cases are answered.
    let add tactic case =

        match tactic, case.GoalOpt with

            | Exact hp, Some p
                when case.Terms.Contains(hp) && hp.Type = p ->
                [ { case with GoalOpt = None } ]

            | Intro hp, Some (Function (p, q))
                when hp.Type = p ->
                [
                    {
                        GoalOpt = Some q
                        Terms = case.Terms.Add(hp)
                    }
                ]

            | Apply (Term.Function (p, q)), Some goal ->
                match apply p q goal with
                    | [] -> None
                    | [typ] -> Some typ
                    | types -> Some (Product types)
                    |> Option.map (fun goal' ->
                        { case with GoalOpt = Some goal' })
                    |> Option.toList

            | Dissolve (Term.Product types as hp), _ ->
                let terms =
                    let newTerms =
                        types
                            |> Seq.map Term.create
                            |> set
                    case.Terms
                        |> Set.remove hp
                        |> Set.union newTerms
                [ { case with Terms = terms } ]

            | Cases (Term.Sum types as hp), _ ->
                let terms = Set.remove hp case.Terms
                types
                    |> List.map (fun typ ->
                        { case with
                            Terms =
                                Set.add (Term.create typ) terms })

            | Left, Some (Sum (P :: _)) ->
                [ { case with GoalOpt = Some P }]

            | Right, Some (Sum (_ :: Q :: [])) ->
                [ { case with GoalOpt = Some Q }]

            | Split, Some (Product types) ->
                types
                    |> List.map (fun typ ->
                        { case with GoalOpt = Some typ })

            | _ -> []

    /// Can the given tactic be added to the given case?
    let canAdd tactic case =
        not (add tactic case).IsEmpty
