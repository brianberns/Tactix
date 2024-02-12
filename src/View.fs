namespace Tactix

open Fable.Core.JsInterop
open Fable.SimpleJson
open Feliz

module View =

    // https://github.com/facebook/react/issues/4431
    type private Browser.Types.DragEvent with
        member evt.OffsetX : float = evt?nativeEvent?offsetX
        member evt.OffsetY : float = evt?nativeEvent?offsetY

    let private format = "application/json"

    let private renderTactics tactics =
        Html.div [
            prop.className "tactics"
            prop.children [
                for tactic in tactics do
                    Html.span [
                        let className =
                            (string tactic).ToLower()
                        prop.className className
                        prop.text (string tactic)
                    ]
            ]
        ]

    let render (model : Model) (dispatch : Msg -> unit) =
        Html.div [
            Html.div [
                prop.className "goal"
            ]
            Html.div [
                prop.className "hypotheses"
            ]
            renderTactics model.Tactics
        ]
