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

    let private renderGoal (goal : Type) =
        Html.div [
            prop.className "goal-area"
            prop.children [
                Html.div [
                    prop.classes [
                        "type"
                        $"{(string goal).ToLower()}"
                    ]
                ]
            ]
        ]

    let private renderTerms (terms : seq<Term>) =
        Html.div [
            prop.className "terms-area"
            prop.children [
                for term in terms do
                    Html.div [
                        prop.classes [
                            "term"
                            $"{(string term).ToLower()}"
                        ]
                    ]
            ]
        ]

    let private renderTactics (tactics : seq<Tactic>) =
        Html.div [
            prop.className "tactics-area"
            prop.children [
                for tactic in tactics do
                    Html.div [
                        prop.className "tactic"
                        prop.text (string tactic)
                        prop.draggable true
                    ]
            ]
        ]

    let render (model : Model) (dispatch : Msg -> unit) =
        Html.div [
            renderGoal model.Goal
            renderTerms model.Terms
            renderTactics model.Tactics
        ]
