﻿namespace Tactix

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
