namespace Tactix

/// One case in a proof.
type ProofCase =
    {
        /// Propositions to be proved. These are implicitly or-ed.
        Goals : Set<Value>

        /// Given hypotheses. These are implicitly and-ed.
        Terms : Set<Term>

        /// Has one of the goals been proved?
        IsComplete : bool
    }

module ProofCase =

    /// Applies p -> q with the given goals, if possible.
    let private apply p q (goals : Set<Value>) =

        let rec loop p q =
            if goals.Contains(Boolean p) then Some ([p], q)
            else
                match q with
                    | Implication (qIn, qOut) ->
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

            | Exact term
                when case.Terms.Contains(term)
                    && case.Goals.Contains(term.Value) ->
                [
                    {
                        case with
                            Goals =
                                case.Goals.Remove(term.Value)
                            IsComplete = true
                    }
                ]

            | Intro (Implication (p, q) as impl)
                when case.Goals.Contains(Boolean impl) ->
                [
                    {
                        case with
                            Goals =
                                case.Goals
                                    .Remove(Boolean impl)
                                    .Add(Boolean q)
                            Terms =
                                { Value = Boolean p }
                                    |> case.Terms.Add
                    }
                ]

            | Apply (Term.Function (p, q) as term)
                when case.Terms.Contains(term) ->
                match apply p q case.Goals with
                    | None -> None
                    | Some ([newBool], oldBool) ->
                        Some (newBool, oldBool)
                    | Some (newBools, oldBool) ->
                        Some (And newBools, oldBool)
                    |> Option.map (fun (newBool, oldBool) ->
                        { case with
                            Goals =
                                case.Goals
                                    .Remove(Boolean oldBool)
                                    .Add(Boolean newBool) })
                    |> Option.toList

            | DissolveGoal (Or bools as oldBool)
                when case.Goals.Contains(Boolean oldBool) ->
                let newGoals =
                    bools
                        |> Seq.map Boolean
                        |> set
                [
                    {
                        case with
                            Goals =
                                case.Goals
                                    |> Set.remove (Boolean oldBool)
                                    |> Set.union newGoals
                    }
                ]

            | DissolveTerm (Term.Product bools as oldTerm)
                when case.Terms.Contains(oldTerm) ->
                let terms =
                    let newTerms =
                        bools
                            |> Seq.map (Boolean >> Term.create)
                            |> set
                    case.Terms
                        |> Set.remove oldTerm
                        |> Set.union newTerms
                [ { case with Terms = terms } ]

            | SplitTerm (Term.Sum bools as oldTerm)
                when case.Terms.Contains(oldTerm) ->
                let terms = Set.remove oldTerm case.Terms
                bools
                    |> List.map (fun bool ->
                        let term = Term.create (Boolean bool)
                        { case with
                            Terms = Set.add term terms })

            | SplitGoal ((And bools) as oldBool)
                when case.Goals.Contains(Boolean oldBool) ->
                let goals = case.Goals.Remove(Boolean oldBool)
                bools
                    |> List.map (fun bool ->
                        { case with
                            Goals = goals.Add(Boolean bool) })

            | AffirmGoal ((Not bool) as oldBool)
                when case.Goals.Contains(Boolean oldBool) ->
                let newTerm = { Value = Boolean bool }
                [
                    {
                        case with
                            Goals = case.Goals.Remove(Boolean oldBool)
                            Terms = case.Terms.Add(newTerm)
                    }
                ]

            | AffirmTerm ((Term.Not bool) as oldTerm)
                when case.Terms.Contains(oldTerm) ->
                [
                    {
                        case with
                            Goals = case.Goals.Add(Boolean bool)
                            Terms = case.Terms.Remove(oldTerm)
                    }
                ]

            | _ -> []

    /// Can the given tactic be added to the given case?
    let canAdd tactic case =
        not (add tactic case).IsEmpty
