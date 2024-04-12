namespace Tactix

open Feliz

module Footer =

    /// Renders footer information.
    let render settings dispatch =
        Html.div [
            prop.id "footer"
            prop.children [

                    // audio on/off
                Html.img [
                    prop.className "settings-button"
                    if settings.AudioEnabled then "https://neal.fun/infinite-craft/sound.svg"
                    else "https://neal.fun/infinite-craft/mute.svg"
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
