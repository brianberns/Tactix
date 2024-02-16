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

/// A term is an instance of a type and proves the corresponding
/// proposition. E.g. HP : P.
type Term =
    {
        /// Type of this term.
        Type : Type
    }

module Term =

    /// Creates a term of the given type.
    let create typ =
        { Type = typ }

    let (|Function|_|) term =
        match term.Type with
            | Function (p, q) -> Some (p, q)
            | _ -> None

    let (|Product|_|) term =
        match term.Type with
            | Product types -> Some types
            | _ -> None

    let (|Sum|_|) term =
        match term.Type with
            | Sum types -> Some types
            | _ -> None

[<RequireQualifiedAccess>]
type TacticType =
    | Exact
    | Intro
    | Apply
    | Cases

type Tactic =
    | Exact of Term
    | Intro of Term
    | Apply of Term
    | Cases of Term

    member tactic.Type =
        match tactic with
            | Exact _ -> TacticType.Exact
            | Intro _ -> TacticType.Intro
            | Apply _ -> TacticType.Apply
            | Cases _ -> TacticType.Cases

type Proof =
    {
        GoalOpt : Option<Type>
        Terms : Set<Term>
    }

module Proof =

    let tryAdd tactic proof =

        match tactic, proof.GoalOpt with

                // term (HP : P) eliminates goal P
            | Exact hp, Some p
                when proof.Terms.Contains(hp)
                    && hp.Type = p ->
                Some { proof with GoalOpt = None }

                // introduces term (HP : P) when goal is P -> Q, changing goal to just Q
            | Intro hp, Some (Function (p, q))
                when hp.Type = p ->
                Some {
                    GoalOpt = Some q
                    Terms = proof.Terms.Add(hp)
                }

                // applies term (HPQ : P -> Q) when goal is Q, changing goal to just P
            | Apply (Term.Function (p, q')), Some q
                when q' = q ->
                Some { proof with GoalOpt = Some p }

            | Cases (Term.Product types as hp), _ ->
                let terms =
                    let newTerms =
                        types
                            |> Seq.map Term.create
                            |> set
                    proof.Terms
                        |> Set.remove hp
                        |> Set.union newTerms
                Some { proof with Terms = terms }

            | _ -> None
