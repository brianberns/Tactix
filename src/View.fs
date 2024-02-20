namespace Tactix

open Browser.Types
open Fable.SimpleJson
open Feliz

type private DragData =
    {
        TacticType : TacticType
    }

module private DragData =

    let private format = "application/json"

    let setData dragData (evt : DragEvent) =
        evt.dataTransfer.setData(
            format, Json.serialize<DragData> dragData)
            |> ignore

    let getData (evt : DragEvent) =
        evt.dataTransfer.getData(format)
            |> Json.parseAs<DragData>

    let tacticType evt =
        (getData evt).TacticType

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

    // https://stackoverflow.com/questions/40940288/drag-datatransfer-data-unavailable-in-ondragover-event
    let private renderDragDrop highlightMsg allow audioEnabled dispatch =
        [
            prop.onDragEnter (fun evt ->
                evt.preventDefault()
                dispatch highlightMsg)

            prop.onDragOver (fun evt ->
                evt.preventDefault())

            prop.onDragLeave (fun evt ->
                evt.preventDefault()
                dispatch Message.noHighlight)

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

    let private allowAny allowTactics term evt =
        allowTactics
            |> Seq.tryPick (fun allowTactic ->
                allowTactic term evt)

    let private renderGoal (caseKey, case) model dispatch =
        assert(model.Proof.CaseMap[caseKey] = case)

        let allowIntro goal evt =
            option {
                if DragData.tacticType evt = TacticType.Intro then
                    let! p =
                        match goal with
                            | Function (p, _) -> Some p
                            | _ -> None
                    let tactic = Intro (Term.create p)
                    if ProofCase.canAdd tactic case then
                        return AddTactic (tactic, caseKey)
            }

        let allowLeftRight (tactic : Tactic) goal evt =
            option {
                if DragData.tacticType evt = tactic.Type then
                    match goal with
                        | Sum _ ->
                            if ProofCase.canAdd tactic case then
                                return AddTactic (tactic, caseKey)
                        | _ -> ()
            }

        let allowSplit goal evt =
            option {
                if DragData.tacticType evt = TacticType.Split then
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

    let private allow (tactic : Tactic) (caseKey, case) evt =
        option {
            if DragData.tacticType evt = tactic.Type then
                if ProofCase.canAdd tactic case then
                    return AddTactic (tactic, caseKey)
        }

    let private renderTerms
        ((caseKey, case) as casePair)
        model
        dispatch =

        let allowExact term =
            allow (Exact term) casePair

        let allowApply term =
            allow (Apply term) casePair

        let allowCases term =
            allow (Cases term) casePair

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

    let private renderProof model dispatch =
        Html.div [
            prop.id "proof"
            prop.children [
                for casePair in Map.toSeq model.Proof.CaseMap do
                    Html.div [
                        prop.className "proof-case"
                        prop.children [
                            renderGoal casePair model dispatch
                            renderTerms casePair model dispatch
                        ]
                    ]
            ]
        ]

    let private renderTacticTypes levelIdx draggable dispatch =
        let tacticTypes =
            Level.levels[levelIdx].TacticTypes
        Html.div [
            prop.id "tactics"
            prop.children [
                for tacticType in tacticTypes do
                    Html.div [
                        prop.className "tactic"
                        prop.text (TacticType.emoji tacticType)
                        prop.title (TacticType.instructions tacticType)
                        if draggable then
                            prop.draggable true
                            prop.onDragStart (
                                DragData.setData
                                    { TacticType = tacticType })
                    ]
                Html.button [
                    prop.text "Expand"
                    prop.onClick (fun _ -> dispatch ExpandAliases)
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
                renderTacticTypes
                    levelIdx
                    (not <| Proof.isComplete model.Proof)
                    dispatch
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
