namespace Tactix

type AllowFunc<'t, 'action> = 't -> 'action -> Option<Message>

module Allow =

    let rec private containsAlias = function
        | Primitive _ -> false
        | Function (P, Q) ->
            List.exists containsAlias [P; Q]
        | Product types
        | Sum types ->
            List.exists containsAlias types
        | Alias _ -> true

    module Goal =

        let intro (caseKey, case) : AllowFunc<_, _> =
            fun goal action ->
                option {
                    if action = GoalAction.Intro then
                        match goal with
                            | Function (p, _) ->
                                let tactic = Intro (Term.create p)
                                if ProofCase.canAdd tactic case then
                                    return AddTactic (tactic, caseKey)
                            | _ -> ()
                }

        let left (caseKey, case) : AllowFunc<_, _> =
            fun goal action ->
                option {
                    if action = GoalAction.Left then
                        match goal with
                            | Sum _ ->
                                let tactic = Left
                                if ProofCase.canAdd tactic case then
                                    return AddTactic (tactic, caseKey)
                            | _ -> ()
                }

        let right (caseKey, case) : AllowFunc<_, _> =
            fun goal action ->
                option {
                    if action = GoalAction.Right then
                        match goal with
                            | Sum _ ->
                                let tactic = Right
                                if ProofCase.canAdd tactic case then
                                    return AddTactic (tactic, caseKey)
                            | _ -> ()
                }

        /// A Cases action becomes a Split tactic when applied to a
        /// Product goal. Each sub-goal must be proved separately.
        let cases (caseKey, case) : AllowFunc<_, _> =
            fun goal action ->
                option {
                    if action = GoalAction.Cases then
                        match goal with
                            | Product _ ->
                                let tactic = Split
                                if ProofCase.canAdd tactic case then
                                    return AddTactic (tactic, caseKey)
                            | _ -> ()
                }

        let expand _ : AllowFunc<_, _> =
            fun goal action ->
                option {
                    if action = GoalAction.Expand then
                        if containsAlias goal then
                            return ExpandAliases
                }

    module Term =

        let exact (caseKey, case) : AllowFunc<_, _> =
            fun term action ->
                option {
                    if action = TermAction.Exact then
                        let tactic = Exact term
                        if ProofCase.canAdd tactic case then
                            return AddTactic (tactic, caseKey)
                }

        /// A Dissolve action becomes a Cases tactic when applied to a
        /// Product term. Each sub-term becomes a separate term, but
        /// no new proof cases are created.
        let dissolve (caseKey, case) : AllowFunc<_, _> =
            fun term action ->
                option {
                    if action = TermAction.Dissolve then
                        match term.Type with
                            | Product _ ->
                                let tactic = Cases term
                                if ProofCase.canAdd tactic case then
                                    return AddTactic (tactic, caseKey)
                            | _ -> ()
                }

        let apply (caseKey, case) : AllowFunc<_, _> =
            fun term action ->
                option {
                    if action = TermAction.Apply then
                        let tactic = Apply term
                        if ProofCase.canAdd tactic case then
                            return AddTactic (tactic, caseKey)
                }

        let cases (caseKey, case) : AllowFunc<_, _> =
            fun term action ->
                option {
                    if action = TermAction.Cases then
                        match term.Type with
                            | Sum _ ->
                                let tactic = Cases term
                                if ProofCase.canAdd tactic case then
                                    return AddTactic (tactic, caseKey)
                            | _ -> ()
                }

        let expand _ : AllowFunc<_, _> =
            fun term action ->
                option {
                    if action = TermAction.Expand then
                        if containsAlias term.Type then
                            return ExpandAliases
                }

    let any allowFuncs : AllowFunc<_, _> =
        fun arg action ->
            allowFuncs
                |> Seq.tryPick (fun (allowFunc : AllowFunc<_, _>) ->
                    allowFunc arg action)
