namespace Tactix

// https://www.ma.imperial.ac.uk/~buzzard/lean_together/source/contents.html

/// A type corresponds to a proposition that might be provable.
type Type =

    /// Atomic proposition, such as P.
    | Primitive of name : string

    /// Implication, such as P → Q.
    | Function of Type * Type

    /// Conjuction (and), such as P ∧ Q.
    | Product of List<Type>

    /// Disjuction (or), such as P ∨ Q.
    | Sum of List<Type>

    override typ.ToString() =

        let toString sep types =
            types
                |> Seq.map string
                |> String.concat (string sep)

        match typ with
            | Primitive name -> name
            | Function (P, Q) -> $"({P}→{Q})"
            | Product types -> $"({toString '∧' types})"
            | Sum types -> $"({toString '∨' types})"

/// A term is an instance of a type and proves the corresponding
/// proposition. E.g. HP : P.
type Term =
    {
        /// Type of this term.
        Type : Type
    }

    override term.ToString() =
        $"H{term.Type}"

module Term =

    /// Creates a term of the given type.
    let create typ =
        { Type = typ }

    /// Matches a function term.
    let (|Function|_|) term =
        match term.Type with
            | Function (p, q) -> Some (p, q)
            | _ -> None

    /// Matches a product term.
    let (|Product|_|) term =
        match term.Type with
            | Product types -> Some types
            | _ -> None

    /// Matches a sum term.
    let (|Sum|_|) term =
        match term.Type with
            | Sum types -> Some types
            | _ -> None

/// Tactic type.
[<RequireQualifiedAccess>]
type TacticType =
    | Exact
    | Intro
    | Apply
    | Cases

type Tactic =

    /// Term (HP : P) eliminates goal P.
    | Exact of Term

    /// Introduces term (HP : P) when goal is P -> Q, changing
    /// goal to just Q.
    | Intro of Term

    /// Applies term (HPQ : P -> Q) when goal is Q, changing
    /// goal to just P.
    | Apply of Term

    | Cases of Term

    /// Tactic type.
    member tactic.Type =
        match tactic with
            | Exact _ -> TacticType.Exact
            | Intro _ -> TacticType.Intro
            | Apply _ -> TacticType.Apply
            | Cases _ -> TacticType.Cases

type ProofCase =
    {
        /// Proposition to be proved.
        GoalOpt : Option<Type>

        /// Hypotheses.
        Terms : Set<Term>
    }

module ProofCase =

    /// Adds the given tactic to the given proof case.
    let add tactic case =

        match tactic, case.GoalOpt with

            | Exact hp, Some p
                when case.Terms.Contains(hp) && hp.Type = p ->
                [ { case with GoalOpt = None } ]

            | Intro hp, Some (Function (p, q))
                when hp.Type = p ->
                [
                    {
                        GoalOpt = Some q
                        Terms = case.Terms.Add(hp)
                    }
                ]

            | Apply (Term.Function (p, q')), Some q
                when q' = q ->
                [ { case with GoalOpt = Some p } ]

            | Cases (Term.Product types as hp), _ ->
                let terms =
                    let newTerms =
                        types
                            |> Seq.map Term.create
                            |> set
                    case.Terms
                        |> Set.remove hp
                        |> Set.union newTerms
                [ { case with Terms = terms } ]

            | Cases (Term.Sum types as hp), _ ->
                let terms = Set.remove hp case.Terms
                types
                    |> List.map (fun typ ->
                        { case with
                            Terms = Set.add (Term.create typ) terms })

            | _ -> []

    /// Can the given tactic be added to the given case?
    let canAdd tactic case =
        (add tactic case).IsEmpty |> not

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
