namespace Tactix

open Feliz

module View =

    /// Renders the given model.
    let render model dispatch =
        Html.div [

                // child HTML elements
            let levelIdx = model.Settings.LevelIndex
            let isActive =
                not <| Proof.isComplete model.Proof
            prop.children [
                Header.render model.Proof levelIdx
                TacticView.renderGoalTactics levelIdx isActive
                ProofView.render model dispatch
                TacticView.renderTermTactics levelIdx isActive
                Footer.render
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
