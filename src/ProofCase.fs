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
            if goals.Contains(Proposition q) then Some ([p], q)
            else
                match q with
                    | Impliction (qIn, qOut) ->
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
                    && case.Goals.Contains(term.Type) ->
                [
                    {
                        case with
                            Goals =
                                case.Goals.Remove(term.Type)
                            IsComplete = true
                    }
                ]

            | Intro (Impliction (p, q) as goal)
                when case.Goals.Contains(Proposition goal) ->
                [
                    {
                        case with
                            Goals =
                                case.Goals
                                    .Remove(Proposition goal)
                                    .Add(Proposition q)
                            Terms =
                                { Type = Proposition p }
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
                        Some (Conjunction newGoals, oldGoal)
                    |> Option.map (fun (newGoal, oldGoal) ->
                        { case with
                            Goals =
                                case.Goals
                                    .Remove(Proposition oldGoal)
                                    .Add(Proposition newGoal) })
                    |> Option.toList

            | DissolveGoal (Disjunction props as oldGoal)
                when case.Goals.Contains(Proposition oldGoal) ->
                let newGoals =
                    props
                        |> Seq.map Proposition
                        |> set
                [
                    {
                        case with
                            Goals =
                                case.Goals
                                    |> Set.remove (Proposition oldGoal)
                                    |> Set.union newGoals
                    }
                ]

            | DissolveTerm (Term.Product props as oldTerm)
                when case.Terms.Contains(oldTerm) ->
                let terms =
                    let newTerms =
                        props
                            |> Seq.map (Proposition >> Term.create)
                            |> set
                    case.Terms
                        |> Set.remove oldTerm
                        |> Set.union newTerms
                [ { case with Terms = terms } ]

            | SplitTerm (Term.Sum props as oldTerm)
                when case.Terms.Contains(oldTerm) ->
                let terms = Set.remove oldTerm case.Terms
                props
                    |> List.map (fun prop ->
                        let term = Term.create (Proposition prop)
                        { case with
                            Terms = Set.add term terms })

            | SplitGoal ((Conjunction props) as oldGoal)
                when case.Goals.Contains(Proposition oldGoal) ->
                let goals = case.Goals.Remove(Proposition oldGoal)
                props
                    |> List.map (fun prop ->
                        { case with
                            Goals = goals.Add(Proposition prop) })

            | AffirmGoal ((Not prop) as oldGoal)
                when case.Goals.Contains(Proposition oldGoal) ->
                let newTerm = { Type = Proposition prop }
                [
                    {
                        case with
                            Goals = case.Goals.Remove(Proposition oldGoal)
                            Terms = case.Terms.Add(newTerm)
                    }
                ]

            | AffirmTerm ((Term.Not prop) as oldTerm)
                when case.Terms.Contains(oldTerm) ->
                [
                    {
                        case with
                            Goals = case.Goals.Add(Proposition prop)
                            Terms = case.Terms.Remove(oldTerm)
                    }
                ]

            | _ -> []

    /// Can the given tactic be added to the given case?
    let canAdd tactic case =
        not (add tactic case).IsEmpty
