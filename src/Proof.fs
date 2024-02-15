namespace Tactix

// https://www.ma.imperial.ac.uk/~buzzard/lean_together/source/contents.html

/// A type corresponds to a proposition that might be provable.
/// E.g. P -> Q.
type Type =
    | Primitive of name : string
    | Function of Type * Type

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

type Tactic =
    | Exact of Term
    | Intro of Term
    | Apply of Term

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
            | Apply hpq, Some q ->
                match hpq.Type with
                    | Function (p, q') when q' = q ->
                        Some { proof with GoalOpt = Some p }
                    | _ -> None

            | _ -> None
