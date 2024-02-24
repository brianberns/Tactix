namespace Tactix

open Browser.Types
open Feliz

type private DragData<'action> =
    {
        Action : 'action
    }

// https://stackoverflow.com/questions/40940288/drag-datatransfer-data-unavailable-in-ondragover-event
// https://stackoverflow.com/questions/31915653/how-to-get-data-from-datatransfer-getdata-in-event-dragover-or-dragenter
type private DragDataManager<'action> =
    {
        mutable Shared : Option<DragData<'action>>
    }

module private DragDataManager =

    let setData dragData (evt : DragEvent) mgr =
        mgr.Shared <- Some dragData

    let getData (evt : DragEvent) mgr =
        mgr.Shared

    let action evt mgr =
        getData evt mgr
            |> Option.map (fun dragData ->
                dragData.Action)

    let goal : DragDataManager<GoalAction> = { Shared = None }
    let term : DragDataManager<TermAction> = { Shared = None }

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

    let private renderGoalActions levelIdx draggable =
        let actions =
            Level.levels[levelIdx].GoalActions
        Html.div [
            prop.id "goal-actions"
            prop.children [
                for action in actions do
                    Html.div [
                        prop.className "action"
                        prop.text (GoalAction.emoji action)
                        prop.title (GoalAction.instructions action)
                        if draggable then
                            prop.draggable true
                            prop.onDragStart (fun evt ->
                                DragDataManager.setData
                                    { Action = action }
                                    evt
                                    DragDataManager.goal)
                    ]
            ]
        ]

    let private renderInnerType typ =

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
                            "primitive-type"
                            name.ToLower()
                        ]
                    | Function (p, q) ->
                        prop.className "compound-type"
                        between "→" loop [p; q]
                            |> prop.children
                    | Product types ->
                        prop.className "compound-type"
                        between Text.andHtml loop types
                            |> prop.children
                    | Sum types ->
                        prop.className "compound-type"
                        between Text.orHtml loop types
                            |> prop.children
                    | Alias (_, [inner], _)
                        when typ = Type.not inner ->
                        prop.className "compound-type"
                        prop.children [
                            Html.span [
                                prop.innerHtml Text.notHtml ]
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

        let children = renderInnerType typ
        let dragDrop =
            let highlightMsg =
                Message.highlightType typ caseKey
            renderDragDrop
                highlightMsg
                allow
                model.Settings.AudioEnabled
                dispatch
        let isHighlighted =
            model.IsHighlighted(typ, caseKey)
        let isPrimitive = Type.isPrimitive typ
        Html.div [
            match isHighlighted, isPrimitive with
                | true, true -> "primitive-type-highlight"
                | true, false -> "compound-type-highlight"
                | false, _ -> "type"
                |> prop.className
            prop.children children
            yield! dragDrop
        ]

    let private renderGoal
        ((caseKey, case) as casePair)
        model
        dispatch =
        assert(model.Proof.CaseMap[caseKey] = case)

        let allowMulti goal evt =
            option {
                let! action =
                    DragDataManager.action
                        evt
                        DragDataManager.goal
                return! Allow.any [
                    Allow.Goal.intro casePair
                    Allow.Goal.left casePair
                    Allow.Goal.right casePair
                    Allow.Goal.cases casePair
                ] goal action
            }

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

        let children = renderInnerType term.Type
        let dragDrop =
            let highlightMsg =
                Message.highlightTerm term caseKey
            renderDragDrop
                highlightMsg
                allow
                model.Settings.AudioEnabled
                dispatch
        let isHighlighted =
            model.IsHighlighted(term, caseKey)
        let isPrimitive = Type.isPrimitive term.Type
        Html.div [
            match isHighlighted, isPrimitive with
                | true, true -> "primitive-term-highlight"
                | true, false -> "compound-term-highlight"
                | false, _ -> "term"
                |> prop.className
            prop.children children
            yield! dragDrop
        ]

    let private renderTerms
        ((caseKey, case) as casePair)
        model
        dispatch =
        assert(model.Proof.CaseMap[caseKey] = case)

        let allowMulti term evt =
            option {
                let! action =
                    DragDataManager.action
                        evt
                        DragDataManager.term
                return! Allow.any [
                    Allow.Term.exact casePair
                    Allow.Term.apply casePair
                    Allow.Term.cases casePair
                    Allow.Term.dissolve casePair
                ] term action
            }

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

        Html.div [
            prop.className "proof-case"
            prop.children [
                renderGoal casePair model dispatch
                renderTerms casePair model dispatch
            ]
        ]

    let private renderProof model dispatch =
        Html.div [
            prop.id "proof"
            prop.children [
                for casePair in Map.toSeq model.Proof.CaseMap do
                    renderProofCase casePair model dispatch
            ]
        ]

    let private renderTermActions levelIdx draggable =
        let actions =
            Level.levels[levelIdx].TermActions
        Html.div [
            prop.id "term-actions"
            prop.children [
                for action in actions do
                    Html.div [
                        prop.className "action"
                        prop.text (TermAction.emoji action)
                        prop.title (TermAction.instructions action)
                        if draggable then
                            prop.draggable true
                            prop.onDragStart (fun evt ->
                                DragDataManager.setData
                                    { Action = action }
                                    evt
                                    DragDataManager.term)
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
            let isActive =
                not <| Proof.isComplete model.Proof
            prop.children [
                renderHeader model.Proof levelIdx
                renderGoalActions levelIdx isActive
                renderProof model dispatch
                renderTermActions levelIdx isActive
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
