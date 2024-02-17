namespace Tactix

type ProofCaseKey = int

type Proof =
    {
        NextKey : ProofCaseKey
        CaseMap : Map<ProofCaseKey, ProofCase>
    }

module Proof =

    let empty =
        {
            NextKey = 0
            CaseMap = Map.empty
        }

    let add case proof =
        {
            NextKey = proof.NextKey + 1
            CaseMap =
                proof.CaseMap
                    |> Map.add proof.NextKey case
        }

    let remove caseKey proof =
        assert(proof.CaseMap.ContainsKey(caseKey))
        let caseMap =
            proof.CaseMap
                |> Map.remove caseKey
        { proof with CaseMap = caseMap }

    let update caseKey case proof =
        assert(proof.CaseMap.ContainsKey(caseKey))
        let caseMap =
            proof.CaseMap
                |> Map.add caseKey case
        { proof with CaseMap = caseMap }

    let addMany cases proof =
        (proof, cases)
            ||> Seq.fold (fun acc case ->
                add case acc)

    let isComplete proof =
        Map.forall (fun _ case ->
            case.GoalOpt.IsNone) proof.CaseMap
