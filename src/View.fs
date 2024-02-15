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
    let private renderDragDrop highlight allow audioEnabled dispatch =
        [
            prop.onDragEnter (fun evt ->
                evt.preventDefault()
                dispatch highlight)

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

    let private renderType typ isHighlighted allow audioEnabled dispatch =
        Html.div [
            prop.className "type"
            prop.children (renderInnerType isHighlighted typ)
            let highlight = Message.highlightType typ
            yield! renderDragDrop highlight allow audioEnabled dispatch
        ]

    let private renderGoal goalOpt isHighlighted allow audioEnabled dispatch =
        Html.div [
            prop.className "goal-area"
            prop.children [
                match goalOpt with
                    | Some goal -> renderType goal isHighlighted allow audioEnabled dispatch
                    | None -> ()
            ]
        ]

    let private renderTerm term isHighlighted highlight allow audioEnabled dispatch =
        Html.div [
            prop.className "term"
            prop.children (renderInnerType isHighlighted term.Type)
            yield! renderDragDrop highlight allow audioEnabled dispatch
        ]

    let private renderTerms model dispatch =
        Html.div [
            prop.className "terms-area"
            prop.children [
                for term in model.Proof.Terms do
                    let isHighlighted =
                        model.Highlighted = Choice2Of3 term
                    renderTerm
                        term
                        isHighlighted
                        (Message.highlightTerm term)
                        (fun (evt : DragEvent) ->
                            if Some term.Type = model.Proof.GoalOpt
                                && (DragData.getData evt).TacticType = TacticType.Exact then
                                    Some (AddTactic (Exact term))
                            else None)
                        model.AudioEnabled
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
            renderHeader model.LevelIndex
            renderGoal
                model.Proof.GoalOpt
                (model.Highlighted =
                    (model.Proof.GoalOpt
                        |> Option.map Choice3Of3
                        |> Option.defaultValue (Choice1Of3 ())))
                (fun _ -> None)
                model.AudioEnabled
                dispatch
            renderTerms model dispatch
            renderTacticTypes
                model.LevelIndex
                model.Proof.GoalOpt.IsSome
            renderFooter
                model.AudioEnabled
                dispatch
        ]
