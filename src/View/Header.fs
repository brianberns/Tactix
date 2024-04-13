namespace Tactix

open Feliz

module Header =

    /// Renders header information.
    let render levelIdx =
        Html.div [
            prop.id "header"
            prop.children [

                    // level number
                Html.div [
                    prop.id "level-num"
                    prop.text $"Level {levelIdx + 1}"
                ]
            ]
        ]
