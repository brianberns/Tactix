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

    let private renderHeader levelIdx =
        let instructions =
            Level.levels[levelIdx].Instructions
        Html.div [
            prop.id "header"
            prop.children [
                Html.div [
                    prop.id "level-num"
                    prop.text $"Level {levelIdx + 1}"
                ]
                if instructions <> "" then
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

    let private renderType (typ : Type) allow (model : Model) dispatch =
        let children =
            let isHighlighted = model.IsHighlighted(typ)
            renderInnerType isHighlighted typ
        let dragDrop =
            let highlightMsg = Message.highlightType typ
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

    let private renderGoal model dispatch =

        let allowIntro evt =
            option {
                if DragData.tacticType evt = TacticType.Intro then
                    let! p =
                        match model.Proof.GoalOpt with
                            | Some (Function (p, _)) -> Some p
                            | _ -> None
                    let tactic = Intro (Term.create p)
                    let! _ = Proof.tryAdd tactic model.Proof
                    return AddTactic tactic
            }

        Html.div [
            prop.className "goal-area"
            prop.children [
                match model.Proof.GoalOpt with
                    | Some goal ->
                        renderType goal allowIntro model dispatch
                    | None -> ()
            ]
        ]

    let private renderTerm (term : Term) allow (model : Model) dispatch =
        let children =
            let isHighlighted = model.IsHighlighted(term)
            renderInnerType isHighlighted term.Type
        let dragDrop =
            let highlightMsg = Message.highlightTerm term
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

    let private allow (tactic : Tactic) proof evt =
        option {
            if DragData.tacticType evt = tactic.Type then
                let! _ = Proof.tryAdd tactic proof
                return AddTactic tactic
        }

    let private allowAny allowTactics term evt =
        allowTactics
            |> Seq.tryPick (fun allowTactic ->
                allowTactic term evt)

    let private renderTerms model dispatch =

        let allowExact term =
            allow (Exact term) model.Proof

        let allowApply term =
            allow (Apply term) model.Proof

        let allowCases term =
            allow (Cases term) model.Proof

        let allowMulti =
            allowAny [ allowExact; allowApply; allowCases ]

        Html.div [
            prop.className "terms-area"
            prop.children [
                for term in model.Proof.Terms do
                    renderTerm
                        term
                        (allowMulti term)
                        model
                        dispatch
            ]
        ]

    let private renderTacticTypes levelIdx draggable =
        let tacticTypes =
            Level.levels[levelIdx].TacticTypes
        Html.div [
            prop.className "tactics-area"
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
            ]
        ]

    let private renderFooter audioEnabled dispatch =
        Html.div [
            prop.id "footer"
            prop.children [
                Html.img [
                    prop.className "settings-button"
                    if audioEnabled then "https://neal.fun/infinite-craft/sound.svg"
                    else "https://neal.fun/infinite-craft/mute.svg"
                    |> prop.src
                    prop.onClick (fun _ ->
                        dispatch (EnableAudio (not audioEnabled)))
                ]
            ]
        ]

    let render model dispatch =
        Html.div [
            renderHeader model.Settings.LevelIndex
            renderGoal model dispatch
            renderTerms model dispatch
            renderTacticTypes
                model.Settings.LevelIndex
                model.Proof.GoalOpt.IsSome
            renderFooter
                model.Settings.AudioEnabled
                dispatch
        ]
