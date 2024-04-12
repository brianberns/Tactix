namespace Tactix

open Feliz

module TacticView =

    /// Renders the given tactic type as an emoji.
    let private renderTacticType tacticType draggable =
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
    let renderGoalTactics levelIdx draggable =
        let tacticTypes =
            Level.levels[levelIdx].GoalTactics
        Html.div [
            prop.id "goal-tactics"
            prop.children [
                for tacticType in tacticTypes do
                    renderTacticType tacticType draggable
            ]
        ]

    /// Renders tactics that apply to terms.
    let renderTermTactics levelIdx draggable =
        let tacticTypes =
            Level.levels[levelIdx].TermTactics
        Html.div [
            prop.id "term-tactics"
            prop.children [
                for tacticType in tacticTypes do
                    renderTacticType tacticType draggable
            ]
        ]
