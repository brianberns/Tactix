namespace Tactix

open System
open Feliz

module View =

    /// Renders the given instructions.
    let private renderInstructions instructions dispatch =
        Html.div [
            prop.id "instructions"
            prop.text (instructions : string)
            prop.onClick (fun _ ->
                dispatch (SetInstructions ""))
        ]

    /// Renders the given model.
    let render model dispatch =
        Html.div [

                // child HTML elements
            let levelIdx = model.Settings.LevelIndex
            let isActive =
                not <| Proof.isComplete model.Proof
            prop.children [
                Header.render
                    levelIdx
                TacticView.renderGoalTactics
                    levelIdx
                    isActive
                    dispatch
                ProofView.render
                    model
                    dispatch
                TacticView.renderTermTactics
                    levelIdx
                    isActive
                    dispatch
                Footer.render
                    model.Settings
                    dispatch
                if not (String.IsNullOrWhiteSpace(model.Instructions)) then
                    renderInstructions
                        model.Instructions
                        dispatch
            ]
                // easter egg for revisiting levels
            prop.onCut (fun _ ->
                let levelIdx =
                    if levelIdx > 0 then levelIdx - 1
                    else Level.levels.Length - 1
                dispatch (StartLevel levelIdx))
        ]
