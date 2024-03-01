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

    let private rewrite lhs rhs typ =

        let rec loop nat =
            if nat = lhs then rhs
            else
                match nat with
                    | Successor n -> Successor (loop n)
                    | _ -> nat

        match typ with
            | Equal (a, b) -> Equal (loop a, loop b)
            | _ -> typ

    /// Adds the given tactic to the given proof case. If
    /// successful, one or more resulting cases are answered.
    let add tactic case =

        match tactic with

            | Exact term
                when case.Terms.Contains(term)
                    && case.Goals.Contains(term.Type) ->
                [
                    {
                        case with
                            Goals =
                                case.Goals.Remove(term.Type)
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

            | Apply (Term.Function (p, q) as term)
                when case.Terms.Contains(term) ->
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

            | SplitTerm (Term.Sum types as oldTerm)
                when case.Terms.Contains(oldTerm) ->
                let terms = Set.remove oldTerm case.Terms
                types
                    |> List.map (fun typ ->
                        { case with
                            Terms =
                                Set.add (Term.create typ) terms })

            | SplitGoal ((Product types) as oldGoal)
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

            | Reflexivity ((Equal (lhs, rhs)) as goal)
                when case.Goals.Contains(goal)
                    && lhs = rhs ->
                let newGoals = case.Goals.Remove(goal)
                [
                    {
                        case with
                            Goals = newGoals
                            IsComplete = newGoals.IsEmpty
                    }
                ]

            | Rewrite (Term.Equal (lhs, rhs) as term)
                when case.Terms.Contains(term) ->
                let newGoals = 
                    case.Goals
                        |> Set.map (rewrite lhs rhs)
                if newGoals = case.Goals then []
                else [ { case with Goals = newGoals } ]

            | _ -> []

    /// Can the given tactic be added to the given case?
    let canAdd tactic case =
        not (add tactic case).IsEmpty
