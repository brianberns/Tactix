namespace Tactix

type AllowFunc<'t> = 't -> TacticType -> Option<Message>

module Allow =

    let allow makeTactic (caseKey, case) : AllowFunc<_> =
        fun arg tacticType ->
            option {
                let tactic : Tactic = makeTactic arg
                if tacticType = tactic.Type then
                    if ProofCase.canAdd tactic case then
                        return AddTactic (tactic, caseKey)
            }

    let any allowFuncs : AllowFunc<_> =
        fun arg tacticType ->
            allowFuncs
                |> Seq.tryPick (fun (allowFunc : AllowFunc<_>) ->
                    allowFunc arg tacticType)
