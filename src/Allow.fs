namespace Tactix

type AllowFunc<'t> = 't -> ActionType -> Option<Message>

module Allow =

    module Type =

        let intro (caseKey, case) : AllowFunc<_> =
            fun goal actionType ->
                option {
                    if actionType = ActionType.Intro then
                        match goal with
                            | Function (p, _) ->
                                let tactic = Intro (Term.create p)
                                if ProofCase.canAdd tactic case then
                                    return AddTactic (tactic, caseKey)
                            | _ -> ()
                }

        let left (caseKey, case) : AllowFunc<_> =
            fun goal actionType ->
                option {
                    if actionType = ActionType.Left then
                        match goal with
                            | Sum _ ->
                                let tactic = Left
                                if ProofCase.canAdd tactic case then
                                    return AddTactic (tactic, caseKey)
                            | _ -> ()
                }

        let right (caseKey, case) : AllowFunc<_> =
            fun goal actionType ->
                option {
                    if actionType = ActionType.Right then
                        match goal with
                            | Sum _ ->
                                let tactic = Right
                                if ProofCase.canAdd tactic case then
                                    return AddTactic (tactic, caseKey)
                            | _ -> ()
                }

        /// A Cases action becomes a Split tactic when applied to a
        /// Product goal. Each sub-goal must be proved separately.
        let cases (caseKey, case) : AllowFunc<_> =
            fun goal actionType ->
                option {
                    if actionType = ActionType.Cases then
                        match goal with
                            | Product _ ->
                                let tactic = Split
                                if ProofCase.canAdd tactic case then
                                    return AddTactic (tactic, caseKey)
                            | _ -> ()
                }

    module Term =

        let exact (caseKey, case) : AllowFunc<_> =
            fun term actionType ->
                option {
                    if actionType = ActionType.Exact then
                        let tactic = Exact term
                        if ProofCase.canAdd tactic case then
                            return AddTactic (tactic, caseKey)
                }

        /// A Dissolve action becomes a Cases tactic when applied to a
        /// Product term. Each sub-term becomes a separate term, but
        /// no new proof cases are created.
        let dissolve (caseKey, case) : AllowFunc<_> =
            fun term actionType ->
                option {
                    if actionType = ActionType.Dissolve then
                        match term.Type with
                            | Product _ ->
                                let tactic = Cases term
                                if ProofCase.canAdd tactic case then
                                    return AddTactic (tactic, caseKey)
                            | _ -> ()
                }

        let apply (caseKey, case) : AllowFunc<_> =
            fun term actionType ->
                option {
                    if actionType = ActionType.Apply then
                        let tactic = Apply term
                        if ProofCase.canAdd tactic case then
                            return AddTactic (tactic, caseKey)
                }

        let cases (caseKey, case) : AllowFunc<_> =
            fun term actionType ->
                option {
                    if actionType = ActionType.Cases then
                        match term.Type with
                            | Sum _ ->
                                let tactic = Cases term
                                if ProofCase.canAdd tactic case then
                                    return AddTactic (tactic, caseKey)
                            | _ -> ()
                }

    let any allowFuncs : AllowFunc<_> =
        fun arg actionType ->
            allowFuncs
                |> Seq.tryPick (fun (allowFunc : AllowFunc<_>) ->
                    allowFunc arg actionType)
