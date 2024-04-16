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
                Header.render
                    model
                    dispatch
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
            ]
                // easter egg for revisiting levels
            prop.onCut (fun _ ->
                let levelIdx =
                    if levelIdx > 0 then levelIdx - 1
                    else Levels.levels.Length - 1
                dispatch (StartLevel levelIdx))
        ]
