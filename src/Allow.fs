namespace Tactix

type AllowFunc<'t> = 't -> TacticType -> Option<Message>

module Allow =

    let intro (caseKey, case) : AllowFunc<_> =
        fun goal tacticType ->
            option {
                if tacticType = TacticType.Intro then
                    let tactic = Intro goal
                    if ProofCase.canAdd tactic case then
                        return AddTactic (tactic, caseKey)
            }

    let left (caseKey, case) : AllowFunc<_> =
        fun goal tacticType ->
            option {
                if tacticType = TacticType.Left then
                    let tactic = Left goal
                    if ProofCase.canAdd tactic case then
                        return AddTactic (tactic, caseKey)
            }

    let right (caseKey, case) : AllowFunc<_> =
        fun goal tacticType ->
            option {
                if tacticType = TacticType.Right then
                    let tactic = Right goal
                    if ProofCase.canAdd tactic case then
                        return AddTactic (tactic, caseKey)
            }

    let split (caseKey, case) : AllowFunc<_> =
        fun goal tacticType ->
            option {
                if tacticType = TacticType.Split then
                    let tactic = Split goal
                    if ProofCase.canAdd tactic case then
                        return AddTactic (tactic, caseKey)
            }

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
                    let tactic = Dissolve term
                    if ProofCase.canAdd tactic case then
                        return AddTactic (tactic, caseKey)
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
                    let tactic = Cases term
                    if ProofCase.canAdd tactic case then
                        return AddTactic (tactic, caseKey)
            }

    let any allowFuncs : AllowFunc<_> =
        fun arg tacticType ->
            allowFuncs
                |> Seq.tryPick (fun (allowFunc : AllowFunc<_>) ->
                    allowFunc arg tacticType)
