namespace Tactix

/// One case in a proof.
type ProofCase =
    {
        /// Proposition to be proved.
        GoalOpt : Option<Type>

        /// Hypotheses.
        Terms : Set<Term>
    }

module ProofCase =

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

            | Apply (Term.Function (p, q')), Some q
                when q' = q ->
                [ { case with GoalOpt = Some p } ]

            | Cases (Term.Product types as hp), _ ->
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

            | _ -> []

    /// Can the given tactic be added to the given case?
    let canAdd tactic case =
        not (add tactic case).IsEmpty
