﻿namespace Tactix

open Browser.Types
open Feliz

type private DragData =
    {
        ActionType : ActionType
    }

// https://stackoverflow.com/questions/40940288/drag-datatransfer-data-unavailable-in-ondragover-event
// https://stackoverflow.com/questions/31915653/how-to-get-data-from-datatransfer-getdata-in-event-dragover-or-dragenter
module private DragData =

    let mutable shared : Option<DragData> = None

    let setData dragData (evt : DragEvent) =
        shared <- Some dragData

    let getData (evt : DragEvent) =
        shared

    let actionType evt =
        match getData evt with
            | Some dragData -> dragData.ActionType
            | None -> failwith "Unexpected"

module View =

    let private renderHeader proof levelIdx =
        Html.div [
            prop.id "header"
            prop.children [

                Html.div [
                    prop.id "level-num"
                    prop.text $"Level {levelIdx + 1}"
                ]

                let level = Level.levels[levelIdx]
                let instructions = level.Instructions
                if instructions <> "" then
                    let proof' = Level.initializeProof level
                    if proof = proof' then
                        Html.div [
                            prop.id "instructions"
                            prop.text instructions
                        ]
                ]
        ]

    let private renderInnerType isHighlighted typ =

        let between content f types =
            [
                for (i, typ) in Seq.indexed types do
                    if i > 0 then
                        yield Html.span [
                            prop.innerHtml content ]
                    yield f typ
            ]

        let rec loop typ =
            Html.div [
                match typ with
                    | Primitive name ->
                        prop.classes [
                            (if isHighlighted then "primitive-type-highlight"
                             else "primitive-type")
                            name.ToLower()
                        ]
                    | Function (p, q) ->
                        prop.className "compound-type"
                        between "→" loop [p; q]
                            |> prop.children
                    | Product types ->
                        prop.className "compound-type"
                        between "&nbsp;∧&nbsp;" loop types
                            |> prop.children
                    | Sum types ->
                        prop.className "compound-type"
                        between "&nbsp;∨&nbsp;" loop types
                            |> prop.children
                    | Alias (_, [inner], _)
                        when typ = Type.not inner ->
                        prop.className "compound-type"
                        prop.children [
                            Html.span [
                                prop.innerHtml "¬" ]
                            loop inner
                        ]
                    | _ -> failwith "Unexpected"
            ]

        loop typ

    let private renderDragDrop
        highlightMsg
        allow
        audioEnabled
        dispatch =

        [
            if highlightMsg <> Message.noHighlight then

                    // start highlighting the target
                prop.onDragEnter (fun evt ->
                    if allow evt |> Option.isSome then
                        evt.preventDefault()
                        dispatch highlightMsg)

                    // stop highlighting the target
                prop.onDragLeave (fun evt ->
                    if allow evt |> Option.isSome then
                        evt.preventDefault()
                        dispatch Message.noHighlight)

                // allow drop
            prop.onDragOver (fun evt ->
                if allow evt |> Option.isSome then
                    evt.preventDefault())

            prop.onDrop (fun evt ->
                evt.preventDefault()
                match allow evt with
                    | Some msg ->
                        if audioEnabled then Audio.playReward ()
                        msg
                    | None ->
                        if audioEnabled then Audio.playError ()
                        Message.noHighlight
                    |> dispatch)
        ]

    let private renderType
        (typ : Type)
        caseKey
        allow
        (model : Model)
        dispatch =

        let children =
            let isHighlighted =
                model.IsHighlighted(typ, caseKey)
            renderInnerType isHighlighted typ
        let dragDrop =
            let highlightMsg =
                Message.highlightType typ caseKey
            renderDragDrop
                highlightMsg
                allow
                model.Settings.AudioEnabled
                dispatch
        Html.div [
            prop.className "type"
            prop.children children
            yield! dragDrop
        ]

    let private allowAny allowActions arg (evt : DragEvent) =
        allowActions
            |> Seq.tryPick (fun allowAction ->
                allowAction arg evt : Option<Message>)

    let private renderGoal (caseKey, case) model dispatch =
        assert(model.Proof.CaseMap[caseKey] = case)

        let allowIntro goal evt =
            option {
                if DragData.actionType evt = ActionType.Intro then
                    let! p =
                        match goal with
                            | Function (p, _) -> Some p
                            | _ -> None
                    let tactic = Intro (Term.create p)
                    if ProofCase.canAdd tactic case then
                        return AddTactic (tactic, caseKey)
            }

        let allowLeftRight tactic goal evt =
            option {
                if DragData.actionType evt = ActionType.ofTactic tactic then
                    match goal with
                        | Sum _ ->
                            if ProofCase.canAdd tactic case then
                                return AddTactic (tactic, caseKey)
                        | _ -> ()
            }

        let allowSplit goal evt =
            option {
                if DragData.actionType evt = ActionType.Split then
                    match goal with
                        | Product _ ->
                            let tactic = Split
                            if ProofCase.canAdd tactic case then
                                return AddTactic (tactic, caseKey)
                        | _ -> ()
            }

        let allowMulti =
            allowAny [
                allowIntro
                allowLeftRight Left
                allowLeftRight Right
                allowSplit
            ]

        Html.div [
            prop.className "goal"
            prop.children [
                match case.GoalOpt with
                    | Some goal ->
                        renderType
                            goal
                            caseKey
                            (allowMulti goal)
                            model
                            dispatch
                    | None -> ()
            ]
        ]

    let private renderTerm
        (term : Term)
        caseKey
        allow
        (model : Model)
        dispatch =

        let children =
            let isHighlighted =
                model.IsHighlighted(term, caseKey)
            renderInnerType isHighlighted term.Type
        let dragDrop =
            let highlightMsg =
                Message.highlightTerm term caseKey
            renderDragDrop
                highlightMsg
                allow
                model.Settings.AudioEnabled
                dispatch
        Html.div [
            prop.className "term"
            prop.children children
            yield! dragDrop
        ]

    let private allowTactic tactic (caseKey, case) evt =
        option {
            if DragData.actionType evt = ActionType.ofTactic tactic then
                Browser.Dom.console.log($"ProofCase.canAdd {tactic} {case}: {ProofCase.canAdd tactic case}")
                if ProofCase.canAdd tactic case then
                    return AddTactic (tactic, caseKey)
        }

    let private renderTerms
        ((caseKey, case) as casePair)
        model
        dispatch =

        let allowExact term =
            allowTactic (Exact term) casePair

        let allowApply term =
            allowTactic (Apply term) casePair

        let allowCases term =
            allowTactic (Cases term) casePair

        let allowMulti =
            allowAny [ allowExact; allowApply; allowCases ]

        Html.div [
            prop.className "terms"
            prop.children [
                for term in case.Terms do
                    renderTerm
                        term
                        caseKey
                        (allowMulti term)
                        model
                        dispatch
            ]
        ]

    let private renderProofCase casePair model dispatch =

        let allow evt =
            option {
                if DragData.actionType evt = ActionType.Expand then
                    return ExpandAliases
            }

        Html.div [
            prop.className "proof-case"
            prop.children [
                renderGoal casePair model dispatch
                renderTerms casePair model dispatch
            ]
            yield! renderDragDrop   // ugly
                Message.noHighlight
                allow
                model.Settings.AudioEnabled
                dispatch
        ]

    let private renderProof model dispatch =
        Html.div [
            prop.id "proof"
            prop.children [
                for casePair in Map.toSeq model.Proof.CaseMap do
                    renderProofCase casePair model dispatch
            ]
        ]

    let private renderActionTypes levelIdx draggable =
        let actionTypes =
            Level.levels[levelIdx].ActionTypes
        Html.div [
            prop.id "actions"
            prop.children [
                for actionType in actionTypes do
                    Html.div [
                        prop.className "action"
                        prop.text (ActionType.emoji actionType)
                        prop.title (ActionType.instructions actionType)
                        if draggable then
                            prop.draggable true
                            prop.onDragStart (
                                DragData.setData
                                    { ActionType = actionType })
                    ]
            ]
        ]

    let private renderFooter settings dispatch =
        Html.div [
            prop.id "footer"
            prop.children [
                Html.img [
                    prop.className "settings-button"
                    if settings.AudioEnabled then "https://neal.fun/infinite-craft/sound.svg"
                    else "https://neal.fun/infinite-craft/mute.svg"
                    |> prop.src
                    prop.onClick (fun _ ->
                        dispatch (EnableAudio (not settings.AudioEnabled)))
                ]
                Html.img [
                    prop.className "settings-button"
                    prop.src "refresh.svg"
                    prop.onClick (fun _ ->
                        dispatch (StartLevel settings.LevelIndex))
                ]
            ]
        ]

    let render model dispatch =
        Html.div [

            let levelIdx = model.Settings.LevelIndex
            prop.children [
                renderHeader model.Proof levelIdx
                renderProof model dispatch
                renderActionTypes
                    levelIdx
                    (not <| Proof.isComplete model.Proof)
                renderFooter
                    model.Settings
                    dispatch
            ]
                // easter egg for revisiting levels
            prop.onCut (fun _ ->
                let levelIdx =
                    if levelIdx > 0 then levelIdx - 1
                    else Level.levels.Length - 1
                dispatch (StartLevel levelIdx))
        ]
