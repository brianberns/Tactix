namespace Tactix

/// Unique identifier of a case within a proof.
type ProofCaseKey = int

/// A proof consists of multiple cases, each of which
/// must be proved.
type Proof =
    {
        /// Key to be used for the next case in this proof.
        NextKey : ProofCaseKey

        /// Cases in this proof, indexed by key.
        CaseMap : Map<ProofCaseKey, ProofCase>
    }

module Proof =

    /// Proof with no cases.
    let empty =
        {
            NextKey = 0
            CaseMap = Map.empty
        }

    /// Adds the given case to the proof.
    let add case proof =
        {
            NextKey = proof.NextKey + 1
            CaseMap =
                proof.CaseMap
                    |> Map.add proof.NextKey case
        }

    /// Adds the given cases to the proof.
    let addMany cases proof =
        (proof, cases)
            ||> Seq.fold (fun acc case ->
                add case acc)

    /// Removes the case with the given key from the proof.
    let remove caseKey proof =
        assert(proof.CaseMap.ContainsKey(caseKey))
        let caseMap =
            proof.CaseMap
                |> Map.remove caseKey
        { proof with CaseMap = caseMap }

    /// Updates the given case within the given proof, using
    /// the given key.
    let update caseKey case proof =
        assert(proof.CaseMap.ContainsKey(caseKey))
        let caseMap =
            proof.CaseMap
                |> Map.add caseKey case
        { proof with CaseMap = caseMap }

    /// A proof is complete if all of its cases' goals have
    /// been met.
    let isComplete proof =
        Map.forall (fun _ case ->
            case.GoalOpt.IsNone) proof.CaseMap
