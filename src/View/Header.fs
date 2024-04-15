namespace Tactix

open Feliz

module Header =

    let private renderLevelNum levelIdx =
        Html.span [
            prop.id "level-num"
            prop.text $"Level {levelIdx + 1}"
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
    let render levelIdx settings dispatch =
        Html.div [
            prop.id "header"
            prop.children [
                renderLevelNum levelIdx
                renderSettings settings dispatch
            ]
        ]
