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

        let rec loop typ =
            Html.div [
                match typ with
                    | Primitive name ->
                        prop.classes [
                            (if isHighlighted then "primitive-type-highlight"
                             else "primitive-type")
                            name.ToLower()
                        ]
                    | Function (typeA, typeB) ->
                        prop.className "function-type"
                        prop.children [
                            loop typeA
                            Html.text "→"
                            loop typeB
                        ]
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

        let allowIntro (evt : DragEvent) =
            if (DragData.getData evt).TacticType = TacticType.Intro then
                match model.Proof.GoalOpt with
                    | Some (Function (typeA, _)) ->
                        Some (AddTactic (Intro typeA))
                    | _ -> None
            else None

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

    let private renderTerms model dispatch =

        let allowExact term (evt : DragEvent) =
            if (DragData.getData evt).TacticType = TacticType.Exact then
                let tactic = Exact term
                if Proof.canAdd tactic model.Proof then
                    Some (AddTactic tactic)
                else None
            else None

        Html.div [
            prop.className "terms-area"
            prop.children [
                for term in model.Proof.Terms do
                    renderTerm
                        term
                        (allowExact term)
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
