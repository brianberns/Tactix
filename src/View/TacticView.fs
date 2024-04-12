namespace Tactix

open Feliz

module TacticView =

    /// Renders the given tactic type as an emoji.
    let private renderTacticType draggable tacticType =
        Html.div [
            prop.className "tactic"
            prop.text (TacticType.emoji tacticType)
            prop.title (TacticType.instructions tacticType)
            if draggable then
                prop.draggable true
                prop.onDragStart (
                    DragData.setTacticType tacticType)
        ]

    /// Renders tactics that apply to goals.
    let private renderTacticTypes
        (id : string)
        draggable
        tacticTypes =
        Html.div [
            prop.id id
            prop.children [
                for tacticType in tacticTypes do
                    renderTacticType draggable tacticType
            ]
        ]

    /// Renders tactics that apply to goals.
    let renderGoalTactics levelIdx draggable =
        Level.levels[levelIdx].GoalTactics
            |> renderTacticTypes "goal-tactics" draggable

    /// Renders tactics that apply to terms.
    let renderTermTactics levelIdx draggable =
        Level.levels[levelIdx].TermTactics
            |> renderTacticTypes "term-tactics" draggable
