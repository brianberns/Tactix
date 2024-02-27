namespace Tactix

/// One case in a proof.
type ProofCase =
    {
        /// Propositions to be proved. These are implicitly or-ed.
        Goals : Set<Type>

        /// Given hypotheses. These are implicitly and-ed.
        Terms : Set<Term>

        /// Has one of the goals been proved?
        IsComplete : bool
    }

module ProofCase =

    /// Adds the given tactic to the given proof case. If
    /// successful, one or more resulting cases are answered.
    let add tactic case =

        match tactic with

            | Exact hp
                when case.Terms.Contains(hp)
                    && case.Goals.Contains(hp.Type) ->
                [
                    {
                        case with
                            Goals = case.Goals.Remove(hp.Type)
                            IsComplete = true
                    }
                ]

            | Intro (Function (p, q) as goal)
                when case.Goals.Contains(goal) ->
                [
                    {
                        case with
                            Goals =
                                case.Goals
                                    .Remove(goal)
                                    .Add(q)
                            Terms =
                                { Type = p }
                                    |> case.Terms.Add
                    }
                ]

            | Apply (Term.Function (p, q) as oldTerm)
                when case.Terms.Contains(oldTerm) ->
                let terms = case.Terms.Remove(oldTerm)
                let newTerm = { Type = q }
                [
                    {
                        case with
                            Terms = terms
                            Goals = case.Goals.Add(p)
                    }
                    {
                        case with
                            Terms = terms.Add(newTerm)
                    }
                ]

            | DissolveGoal (Sum types as oldGoal)
                when case.Goals.Contains(oldGoal) ->
                let newGoals = set types
                [
                    {
                        case with
                            Goals =
                                case.Goals
                                    |> Set.remove oldGoal
                                    |> Set.union newGoals
                    }
                ]

            | DissolveTerm (Term.Product types as oldTerm)
                when case.Terms.Contains(oldTerm) ->
                let terms =
                    let newTerms =
                        types
                            |> Seq.map Term.create
                            |> set
                    case.Terms
                        |> Set.remove oldTerm
                        |> Set.union newTerms
                [ { case with Terms = terms } ]

            | Cases (Term.Sum types as oldTerm)
                when case.Terms.Contains(oldTerm) ->
                let terms = Set.remove oldTerm case.Terms
                types
                    |> List.map (fun typ ->
                        { case with
                            Terms =
                                Set.add (Term.create typ) terms })

            | Split ((Product types) as oldGoal)
                when case.Goals.Contains(oldGoal) ->
                let goals = case.Goals.Remove(oldGoal)
                types
                    |> List.map (fun typ ->
                        { case with
                            Goals = goals.Add(typ) })

            | AffirmGoal ((Not typ) as oldGoal)
                when case.Goals.Contains(oldGoal) ->
                let newTerm = { Type = typ }
                [
                    {
                        case with
                            Goals = case.Goals.Remove(oldGoal)
                            Terms = case.Terms.Add(newTerm)
                    }
                ]

            | AffirmTerm ((Term.Not typ) as oldTerm)
                when case.Terms.Contains(oldTerm) ->
                [
                    {
                        case with
                            Goals = case.Goals.Add(typ)
                            Terms = case.Terms.Remove(oldTerm)
                    }
                ]

            | _ -> []

    /// Can the given tactic be added to the given case?
    let canAdd tactic case =
        not (add tactic case).IsEmpty
