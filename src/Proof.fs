namespace Tactix

/// Unique identifier of a case within a proof.
type ProofCaseKey = List<int>

/// A proof consists of multiple cases, each of which
/// must be proved.
type Proof =
    {
        /// Total number of cases seen so far.
        NumCases : int

        /// Cases in this proof, indexed by key.
        CaseMap : Map<ProofCaseKey, ProofCase>
    }

module Proof =

    /// Proof with no cases.
    let empty =
        {
            NumCases = 0
            CaseMap = Map.empty
        }

    /// Adds the given case to the proof.
    let add parentKeyOpt case proof =
        let numCases = proof.NumCases + 1
        let key : ProofCaseKey =
            parentKeyOpt
                |> Option.map (fun (parentKey : ProofCaseKey) ->
                    parentKey @ [numCases])
                |> Option.defaultValue [numCases]
        {
            NumCases = numCases
            CaseMap = Map.add key case proof.CaseMap
        }

    /// Adds the given cases to the proof.
    let addMany parentKeyOpt cases proof =
        (proof, cases)
            ||> Seq.fold (fun acc case ->
                add parentKeyOpt case acc)

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
            case.IsComplete) proof.CaseMap
