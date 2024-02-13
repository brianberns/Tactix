namespace Tactix

open Feliz

module View =

    module private Type =

        let className (typ : Type) =
            (string typ).ToLower()

    let private renderGoal (goal : Type) =
        Html.div [
            prop.className "goal-area"
            prop.children [
                Html.div [
                    prop.classes [
                        "type"
                        Type.className goal
                    ]
                ]
            ]
        ]

    module private Term =

        let id (term : Term) =
            $"term-{term.Name}"

    let private renderTerm term goal highlight dispatch =
        Html.div [
            prop.id (Term.id term)
            prop.classes [
                "term"
                if highlight then "term-highlight"
                Type.className term.Type
            ]
            if term.Type = goal then
                prop.onDragEnter (fun evt ->
                    evt.preventDefault()
                    dispatch (HighlightTerm (term.Name, true)))
                prop.onDragOver (fun evt ->
                    evt.preventDefault())
                prop.onDragLeave (fun evt ->
                    evt.preventDefault()
                    dispatch (HighlightTerm (term.Name, false)))
                prop.onDrop (fun evt ->
                    evt.preventDefault())
        ]

    let private renderTerms model dispatch =
        Html.div [
            prop.className "terms-area"
            prop.children [
                for term in model.Proof.Terms do
                    let highlight =
                        model.HighlightedTermNames.Contains(term.Name)
                    renderTerm
                        term
                        model.Proof.Goal
                        highlight
                        dispatch
            ]
        ]

    let private renderTactics (tactics : seq<Tactic>) =
        Html.div [
            prop.className "tactics-area"
            prop.children [
                for tactic in tactics do
                    Html.div [
                        prop.className "tactic"
                        match tactic with
                            | Exact -> "🎆"
                            | Intro -> "🚀"
                            | Apply -> "👣"
                            |> prop.text
                        prop.draggable true
                    ]
            ]
        ]

    let render (model : Model) (dispatch : Msg -> unit) =
        Html.div [
            renderGoal model.Proof.Goal
            renderTerms model dispatch
            renderTactics model.Proof.Tactics
        ]
