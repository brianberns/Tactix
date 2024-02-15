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
    | Intro of Type
    | Apply

type Proof =
    {
        GoalOpt : Option<Type>
        Terms : Set<Term>
    }

module Proof =

    let canAdd tactic proof =
        match tactic, proof.GoalOpt with
            | Exact term, Some goal ->
                proof.Terms.Contains(term) && term.Type = goal
            | Intro typ, Some (Function (typeA, _))
                when typeA = typ -> true
            | _ -> false

    let add tactic proof =
        assert(canAdd tactic proof)
        match tactic, proof.GoalOpt with
            | Exact term, _ ->
                {
                    GoalOpt = None
                    Terms = proof.Terms.Remove(term)
                }
            | Intro typ, Some (Function (_, typeB)) ->
                {
                    GoalOpt = Some typeB
                    Terms =
                        Term.create typ
                            |> proof.Terms.Add
                }
            | _ -> failwith "Unexpected"
    