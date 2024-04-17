namespace Tactix

open System
open Feliz

module Header =

    /// Renders level information.
    let private renderLevelNum settings dispatch =
        Html.div [
            prop.id "level-num"
            prop.children [

                    // level number
                Html.div [
                    prop.text $"Level {settings.LevelIndex + 1}"
                ]

                Html.div [

                        // level back
                    Html.img [
                        prop.className "settings-button"
                        prop.src "back.svg"
                        prop.onClick (fun _ ->
                            dispatch (StartLevel (settings.LevelIndex - 1)))
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
        ]

    /// Renders the given instruction.
    let private renderInstruction instruction =
        Html.span [
            if not (String.IsNullOrWhiteSpace(instruction)) then
                prop.id "instruction"
                prop.innerHtml instruction
        ]

    /// Renders settings.
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
            ]
        ]

    /// Renders header information.
    let render model dispatch =
        Html.div [
            prop.id "header"
            prop.children [
                renderLevelNum model.Settings dispatch
                renderInstruction model.Instruction
                renderSettings model.Settings dispatch
            ]
        ]
