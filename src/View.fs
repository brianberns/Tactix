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
        let instructions = Level.levels[levelIdx].Instructions
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

    let private renderType typ =

        let rec loop (typ : Type) =
            Html.div [
                match typ with
                    | Primitive name ->
                        prop.classes [
                            "type"
                            "primitive-type"
                            name.ToLower()
                        ]
                    | Function (typeA, typeB) ->
                        prop.classes [
                            "type"
                            "function-type"
                        ]
                        prop.children [
                            loop typeA
                            Html.text "→"
                            loop typeB
                        ]
            ]

        Html.div [
            prop.classes [
                "type"
                "top-level-type"
            ]
            prop.children (loop typ)
        ]

    let private renderGoal (goalOpt : Option<Type>) =
        Html.div [
            prop.className "goal-area"
            prop.children [
                match goalOpt with
                    | Some goal -> renderType goal
                    | None -> ()
            ]
        ]

    let private renderTerm term goalOpt highlight audioEnabled dispatch =

        let allowTacticExact (evt : DragEvent) =
            Some term.Type = goalOpt
                && (DragData.getData evt).TacticType = TacticType.Exact

        let rec loop typ =
            Html.div [
                match typ with
                    | Primitive name ->
                        prop.classes [
                            "term"
                            "primitive-term"
                            if highlight then "primitive-term-highlight"
                            name.ToLower()
                        ]
                    | Function (typeA, typeB) ->
                        prop.classes [
                            "term"
                            "function-term"
                        ]
                        prop.children [
                            loop typeA
                            Html.text "→"
                            loop typeB
                        ]
            ]

        Html.div [
            prop.classes [
                "term"
                "top-level-term"
            ]
            prop.children (loop term.Type)

            // https://stackoverflow.com/questions/40940288/drag-datatransfer-data-unavailable-in-ondragover-event

            prop.onDragEnter (fun evt ->
                evt.preventDefault()
                dispatch (Highlight (Choice2Of3 term)))

            prop.onDragOver (fun evt ->
                evt.preventDefault())

            prop.onDragLeave (fun evt ->
                evt.preventDefault()
                dispatch (Highlight (Choice1Of3 ())))

            prop.onDrop (fun evt ->
                evt.preventDefault()
                let msg =
                    if allowTacticExact evt then
                        if audioEnabled then Audio.playReward ()
                        AddTactic (Exact term)
                    else
                        if audioEnabled then Audio.playError ()
                        Highlight (Choice1Of3 ())
                dispatch msg)
        ]

    let private renderTerms model dispatch =
        Html.div [
            prop.className "terms-area"
            prop.children [
                for term in model.Proof.Terms do
                    let highlight =
                        model.Highlighted = Choice2Of3 term
                    renderTerm
                        term
                        model.Proof.GoalOpt
                        highlight
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
            renderGoal model.Proof.GoalOpt
            renderTerms model dispatch
            renderTacticTypes
                model.LevelIndex
                model.Proof.GoalOpt.IsSome
            renderFooter
                model.AudioEnabled
                dispatch
        ]
