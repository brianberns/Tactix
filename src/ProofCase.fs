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

    /// Applies p -> q with the given goals, if possible.
    let private apply p q (goals : Set<Type>) =

        let rec loop p q =
            if goals.Contains(q) then Some ([p], q)
            else
                match q with
                    | Function (qIn, qOut) ->
                        option {
                            let! newGoals, oldGoal = loop qIn qOut
                            return p :: newGoals, oldGoal
                        }
                    | _ -> None

        loop p q

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

            | Apply (Term.Function (p, q)) ->
                match apply p q case.Goals with
                    | None -> None
                    | Some ([newGoal], oldGoal) ->
                        Some (newGoal, oldGoal)
                    | Some (newGoals, oldGoal) ->
                        Some (Product newGoals, oldGoal)
                    |> Option.map (fun (newGoal, oldGoal) ->
                        { case with
                            Goals =
                                case.Goals
                                    .Remove(oldGoal)
                                    .Add(newGoal) })
                    |> Option.toList

            | Dissolve (Term.Product types as hp) ->
                let terms =
                    let newTerms =
                        types
                            |> Seq.map Term.create
                            |> set
                    case.Terms
                        |> Set.remove hp
                        |> Set.union newTerms
                [ { case with Terms = terms } ]

            | Cases (Term.Sum types as hp) ->
                let terms = Set.remove hp case.Terms
                types
                    |> List.map (fun typ ->
                        { case with
                            Terms =
                                Set.add (Term.create typ) terms })

            | Left (Sum (P :: _) as oldGoal) ->
                [
                    {
                        case with
                            Goals =
                                case.Goals
                                    .Remove(oldGoal)
                                    .Add(P)
                    }
                ]

            | Right (Sum (_ :: Q :: []) as oldGoal) ->
                [
                    {
                        case with
                            Goals =
                                case.Goals
                                    .Remove(oldGoal)
                                    .Add(Q)
                    }
                ]

            | Split ((Product types) as oldGoal) ->
                let goals = case.Goals.Remove(oldGoal)
                types
                    |> List.map (fun typ ->
                        { case with
                            Goals = goals.Add(typ) })

            | _ -> []

    /// Can the given tactic be added to the given case?
    let canAdd tactic case =
        not (add tactic case).IsEmpty
