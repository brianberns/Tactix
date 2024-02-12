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

    let private renderGoal (goal : Proposition) =
        Html.div [
            prop.className "goal-area"
            prop.children [
                Html.div [
                    prop.classes [
                        "proposition"
                        $"{(string goal).ToLower()}"
                    ]
                ]
            ]
        ]

    let private renderHypotheses (hypotheses : List<Hypothesis>) =
        Html.div [
            prop.className "hypotheses-area"
            prop.children [
                for hyp in hypotheses do
                    Html.div [
                        prop.classes [
                            "hypothesis"
                            $"{(string hyp).ToLower()}"
                        ]
                    ]
            ]
        ]

    let private renderTactics (tactics : seq<Tactic>) =
        Html.div [
            prop.className "tactics-area"
            prop.children [
                for tactic in tactics do
                    Html.button [
                        prop.className "tactic"
                        prop.text (string tactic)
                    ]
            ]
        ]

    let render (model : Model) (dispatch : Msg -> unit) =
        Html.div [
            renderGoal model.Goal
            renderHypotheses model.Hypotheses
            renderTactics model.Tactics
        ]
