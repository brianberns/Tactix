namespace Tactix

open System
open Feliz

module View =

    let private renderInstructions instructions =
        Html.div [
            prop.id "instructions"
            prop.innerHtml instructions
        ]

    /// Renders the given model.
    let render model dispatch =
        Html.div [

                // child HTML elements
            let levelIdx = model.Settings.LevelIndex
            let isActive =
                not <| Proof.isComplete model.Proof
            prop.children [
                Header.render levelIdx
                TacticView.renderGoalTactics levelIdx isActive
                ProofView.render model dispatch
                TacticView.renderTermTactics levelIdx isActive
                Footer.render
                    model.Settings
                    dispatch
                if not (String.IsNullOrWhiteSpace(model.Instructions)) then
                    renderInstructions model.Instructions
            ]
                // easter egg for revisiting levels
            prop.onCut (fun _ ->
                let levelIdx =
                    if levelIdx > 0 then levelIdx - 1
                    else Level.levels.Length - 1
                dispatch (StartLevel levelIdx))
        ]
