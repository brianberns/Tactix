namespace Tactix

open Feliz

module TacticView =

    /// Renders the given tactic type as an emoji.
    let private renderTacticType
        draggable
        dispatch
        tacticType =
        Html.div [
            prop.className "tactic"
            prop.text (TacticType.emoji tacticType)
            prop.onClick (fun _ ->
                TacticType.instruction tacticType
                    |> SetInstruction
                    |> dispatch)
            if draggable then
                prop.draggable true
                prop.onDragStart (fun evt ->
                    Audio.enable ()
                    DragData.setTacticType tacticType evt)
        ]

    /// Renders tactics that apply to goals.
    let private renderTacticTypes
        id
        draggable
        dispatch
        tacticTypes =
        Html.div [
            prop.id (id : string)
            prop.children [
                for tacticType in tacticTypes do
                    renderTacticType
                        draggable dispatch tacticType
            ]
        ]

    /// Renders tactics that apply to goals.
    let renderGoalTactics
        levelIdx
        draggable
        dispatch =
        Levels.levels[levelIdx].GoalTactics
            |> renderTacticTypes
                "goal-tactics" draggable dispatch

    /// Renders tactics that apply to terms.
    let renderTermTactics
        levelIdx
        draggable
        dispatch =
        Levels.levels[levelIdx].TermTactics
            |> renderTacticTypes
                "term-tactics" draggable dispatch
