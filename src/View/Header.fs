namespace Tactix

open Feliz

module Header =

    /// Renders header information.
    let render proof levelIdx =
        Html.div [
            prop.id "header"
            prop.children [

                    // level number
                Html.div [
                    prop.id "level-num"
                    prop.text $"Level {levelIdx + 1}"
                ]
                    // instructions for this level, if any
                let level = Level.levels[levelIdx]
                let instructions = level.Instructions
                if instructions <> "" then
                    let proof' = Level.initializeProof level
                    if proof = proof' then
                        Html.div [
                            prop.id "instructions"
                            prop.text instructions
                        ]
                ]
        ]
