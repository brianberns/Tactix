namespace Tactix

type AllowFunc<'t> = 't -> TacticType -> Option<Message>

module Allow =

    module Goal =

        let intro (caseKey, case) : AllowFunc<_> =
            fun goal tacticType ->
                option {
                    if tacticType = TacticType.Intro then
                        match goal with
                            | Function (p, _) ->
                                let tactic = Intro (Term.create p)
                                if ProofCase.canAdd tactic case then
                                    return AddTactic (tactic, caseKey)
                            | _ -> ()
                }

        let left (caseKey, case) : AllowFunc<_> =
            fun goal tacticType ->
                option {
                    if tacticType = TacticType.Left then
                        match goal with
                            | Sum _ ->
                                let tactic = Left
                                if ProofCase.canAdd tactic case then
                                    return AddTactic (tactic, caseKey)
                            | _ -> ()
                }

        let right (caseKey, case) : AllowFunc<_> =
            fun goal tacticType ->
                option {
                    if tacticType = TacticType.Right then
                        match goal with
                            | Sum _ ->
                                let tactic = Right
                                if ProofCase.canAdd tactic case then
                                    return AddTactic (tactic, caseKey)
                            | _ -> ()
                }

        let split (caseKey, case) : AllowFunc<_> =
            fun goal tacticType ->
                option {
                    if tacticType = TacticType.Split then
                        match goal with
                            | Product _ ->
                                let tactic = Split
                                if ProofCase.canAdd tactic case then
                                    return AddTactic (tactic, caseKey)
                            | _ -> ()
                }

    module Term =

        let exact (caseKey, case) : AllowFunc<_> =
            fun term tacticType ->
                option {
                    if tacticType = TacticType.Exact then
                        let tactic = Exact term
                        if ProofCase.canAdd tactic case then
                            return AddTactic (tactic, caseKey)
                }

        let dissolve (caseKey, case) : AllowFunc<_> =
            fun term tacticType ->
                option {
                    if tacticType = TacticType.Dissolve then
                        match term.Type with
                            | Product _ ->
                                let tactic = Dissolve term
                                if ProofCase.canAdd tactic case then
                                    return AddTactic (tactic, caseKey)
                            | _ -> ()
                }

        let apply (caseKey, case) : AllowFunc<_> =
            fun term tacticType ->
                option {
                    if tacticType = TacticType.Apply then
                        let tactic = Apply term
                        if ProofCase.canAdd tactic case then
                            return AddTactic (tactic, caseKey)
                }

        let cases (caseKey, case) : AllowFunc<_> =
            fun term tacticType ->
                option {
                    if tacticType = TacticType.Cases then
                        match term.Type with
                            | Sum _ ->
                                let tactic = Cases term
                                if ProofCase.canAdd tactic case then
                                    return AddTactic (tactic, caseKey)
                            | _ -> ()
                }

    let any allowFuncs : AllowFunc<_> =
        fun arg tacticType ->
            allowFuncs
                |> Seq.tryPick (fun (allowFunc : AllowFunc<_>) ->
                    allowFunc arg tacticType)
