﻿namespace Tactix

[<RequireQualifiedAccess>]
type TacticType =
    | Exact
    | Intro
    | Apply

module TacticType =

    let emoji = function
        | TacticType.Exact -> "🎆"
        | TacticType.Intro -> "🚀"
        | TacticType.Apply -> "👣"

type Level =
    {
        Goal : Type
        Terms : Set<Term>
        TacticTypes : Set<TacticType>
        Instructions : string
    }

module Level =

    module private Term =

        let create typ =
            Term.create $"H{typ}" typ

    let private level0 =
        {
            Goal = P
            Terms =
                set [
                    Term.create P
                    Term.create Q
                ]
            TacticTypes =
                set [
                    TacticType.Exact
                ]
            Instructions =
                let exact = TacticType.emoji TacticType.Exact
                $"Drag {exact} onto the symbol that matches the top goal."
        }

    let private level1 =
        {
            Goal = R
            Terms =
                set [
                    Term.create P
                    Term.create Q
                    Term.create R
                ]
            TacticTypes =
                set [
                    TacticType.Exact
                ]
            Instructions = ""
        }

    let levels =
        [|
            level0
            level1
        |]

    let initializeProof level =
        {
            Goal = Some level.Goal
            Terms = level.Terms
        }