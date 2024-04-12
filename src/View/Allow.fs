namespace Tactix

/// A function that determines whether the given argument
/// (a goal or term) allows the given tactic type.
type AllowFunc<'t> = 't -> TacticType -> Option<Message>

module Allow =

    /// Creates an allow function for the given tactic
    /// maker and proof case.
    let allow makeTactic (caseKey, case) : AllowFunc<_> =
        fun arg tacticType ->
            option {
                let tactic : Tactic = makeTactic arg
                if tacticType = tactic.Type then
                    if ProofCase.canAdd tactic case then
                        return AddTactic (tactic, caseKey)
            }

    /// Composes the given allow functions.
    let any allowFuncs : AllowFunc<_> =
        fun arg tacticType ->
            allowFuncs
                |> Seq.tryPick (fun (allowFunc : AllowFunc<_>) ->
                    allowFunc arg tacticType)
