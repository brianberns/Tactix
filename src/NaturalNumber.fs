namespace Tactix

type NaturalNumberVariable = string

type NaturalNumber =
    | Primitive of name : string
    | Zero
    | Variable of NaturalNumberVariable : string
    | Successor of NaturalNumber
    | Addition of NaturalNumber * NaturalNumber

    /// Display string.
    override nat.ToString() =
        match nat with
            | Primitive name -> name
            | Zero -> "0"
            | Variable name -> name
            | Successor a -> $"S({a})"
            | Addition (a, b) -> $"{a}+{b}"

module NaturalNumber =

    let rec freeVariables = function
        | Primitive _
        | Zero -> Set.empty
        | Variable name -> Set.singleton name
        | Successor nat -> freeVariables nat
        | Addition (a, b) -> freeVariables a + freeVariables b

/// Substitute numbers for variables.
type Substitution = Map<NaturalNumberVariable, NaturalNumber>

module Substitution =

    let empty : Substitution =
        Map.empty

    module Map =

        /// Left-biased union of two maps.
        // https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html#v:union
        let union map1 map2 =
            Seq.append (Map.toSeq map2) (Map.toSeq map1)
                |> Map.ofSeq

    let rec apply (subst : Substitution) = function
        | Primitive _ as nat -> nat
        | Zero -> Zero
        | Variable var as nat ->
            subst
                |> Map.tryFind var
                |> Option.map (apply subst)
                |> Option.defaultValue nat
        | Successor nat ->
            Successor (apply subst nat)
        | Addition (a, b) ->
            Addition (apply subst a, apply subst b)

    /// Composition of substitutions.
    let compose (subst1 : Substitution) (subst2 : Substitution) : Substitution =
        subst2
            |> Map.map (fun _ nat ->
                apply subst1 nat)
            |> Map.union subst1

    /// Does the given variable occur in the given number?
    let private occurs var nat =
        Set.contains var
            (NaturalNumber.freeVariables nat)

    let rec tryUnify a b =
        match a, b with

            | Variable var, nat
            | nat, Variable var ->
                if nat = Variable var then
                    Some empty
                elif occurs var nat then None
                else Some (Map [var, nat])

            | Primitive nameA, Primitive nameB
                when nameA = nameB ->
                Some empty

            | Successor natA, Successor natB ->
                tryUnify natA natB

            | Addition (natA1, natA2), Addition (natB1, natB2) ->
                option {
                    let! subst1 = tryUnify natA1 natB1
                    let! subst2 =
                        tryUnify
                            (apply subst1 natA2)
                            (apply subst1 natB2)
                    return subst2
                }

            | _ -> None
