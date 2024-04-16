namespace Tactix

open System
open Feliz

module Header =

    let private renderLevelNum levelIdx =
        Html.span [
            prop.id "level-num"
            prop.text $"Level {levelIdx + 1}"
        ]

    /// Renders the given instruction.
    let private renderInstruction instructionOpt dispatch =
        let text =
            match instructionOpt with
                | Some (LevelInstruction level) ->
                    level.Instruction
                | Some (TacticInstruction tacticType) ->
                    TacticType.instruction tacticType
                | None -> ""
        Html.span [
            prop.id "instruction"
            if not (String.IsNullOrWhiteSpace(text)) then
                prop.text text
                prop.onClick (fun _ ->
                    dispatch (SetInstruction None))
        ]

    /// Renders footer information.
    let private renderSettings settings dispatch =
        Html.span [
            prop.id "settings"
            prop.children [

                    // audio on/off
                Html.img [
                    prop.className "settings-button"
                    if settings.AudioEnabled then "sound.svg"
                    else "mute.svg"
                    |> prop.src
                    prop.onClick (fun _ ->
                        dispatch (
                            EnableAudio (not settings.AudioEnabled)))
                ]
                    // level restart
                Html.img [
                    prop.className "settings-button"
                    prop.src "refresh.svg"
                    prop.onClick (fun _ ->
                        dispatch (StartLevel settings.LevelIndex))
                ]
            ]
        ]

    /// Renders header information.
    let render model dispatch =
        Html.div [
            prop.id "header"
            prop.children [
                renderLevelNum model.Settings.LevelIndex
                renderInstruction model.InstructionOpt dispatch
                renderSettings model.Settings dispatch
            ]
        ]
