namespace Tactix

type AllowFunc<'t, 'action> = 't -> 'action -> Option<Message>

module Allow =

    module Goal =

        let intro (caseKey, case) : AllowFunc<_, _> =
            fun goal actionType ->
                option {
                    if actionType = GoalAction.Intro then
                        match goal with
                            | Function (p, _) ->
                                let tactic = Intro (Term.create p)
                                if ProofCase.canAdd tactic case then
                                    return AddTactic (tactic, caseKey)
                            | _ -> ()
                }

        let left (caseKey, case) : AllowFunc<_, _> =
            fun goal actionType ->
                option {
                    if actionType = GoalAction.Left then
                        match goal with
                            | Sum _ ->
                                let tactic = Left
                                if ProofCase.canAdd tactic case then
                                    return AddTactic (tactic, caseKey)
                            | _ -> ()
                }

        let right (caseKey, case) : AllowFunc<_, _> =
            fun goal actionType ->
                option {
                    if actionType = GoalAction.Right then
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
            fun goal actionType ->
                option {
                    if actionType = GoalAction.Cases then
                        match goal with
                            | Product _ ->
                                let tactic = Split
                                if ProofCase.canAdd tactic case then
                                    return AddTactic (tactic, caseKey)
                            | _ -> ()
                }

    module Term =

        let exact (caseKey, case) : AllowFunc<_, _> =
            fun term actionType ->
                option {
                    if actionType = TermAction.Exact then
                        let tactic = Exact term
                        if ProofCase.canAdd tactic case then
                            return AddTactic (tactic, caseKey)
                }

        /// A Dissolve action becomes a Cases tactic when applied to a
        /// Product term. Each sub-term becomes a separate term, but
        /// no new proof cases are created.
        let dissolve (caseKey, case) : AllowFunc<_, _> =
            fun term actionType ->
                option {
                    if actionType = TermAction.Dissolve then
                        match term.Type with
                            | Product _ ->
                                let tactic = Cases term
                                if ProofCase.canAdd tactic case then
                                    return AddTactic (tactic, caseKey)
                            | _ -> ()
                }

        let apply (caseKey, case) : AllowFunc<_, _> =
            fun term actionType ->
                option {
                    if actionType = TermAction.Apply then
                        let tactic = Apply term
                        if ProofCase.canAdd tactic case then
                            return AddTactic (tactic, caseKey)
                }

        let cases (caseKey, case) : AllowFunc<_, _> =
            fun term actionType ->
                option {
                    if actionType = TermAction.Cases then
                        match term.Type with
                            | Sum _ ->
                                let tactic = Cases term
                                if ProofCase.canAdd tactic case then
                                    return AddTactic (tactic, caseKey)
                            | _ -> ()
                }

    let any allowFuncs : AllowFunc<_, _> =
        fun arg actionType ->
            allowFuncs
                |> Seq.tryPick (fun (allowFunc : AllowFunc<_, _>) ->
                    allowFunc arg actionType)
