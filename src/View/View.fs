namespace Tactix

open System
open Feliz

module View =

    /// Renders the given instruction.
    let private renderInstruction instruction dispatch =
        Html.div [
            prop.id "instruction"
            match instruction with
                | LevelInstruction level ->
                    prop.text level.Instruction
                | TacticInstruction tacticType ->
                    prop.text (TacticType.instruction tacticType)
            prop.onClick (fun _ ->
                dispatch (SetInstruction None))
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
                match model.InstructionOpt with
                    | Some instruction ->
                        renderInstruction
                            instruction
                            dispatch
                    | None -> ()
            ]
                // easter egg for revisiting levels
            prop.onCut (fun _ ->
                let levelIdx =
                    if levelIdx > 0 then levelIdx - 1
                    else Level.levels.Length - 1
                dispatch (StartLevel levelIdx))
        ]
