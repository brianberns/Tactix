namespace Tactix

open Browser.Types
open Fable.SimpleJson
open Feliz

type private DragData =
    {
        TacticType : TacticType
    }

module private DragData =

    let private format = "application/json"

    let setData dragData (evt : DragEvent) =
        Browser.Dom.console.log($"setData: {Json.serialize<DragData> dragData}")
        evt.dataTransfer.setData(
            format, Json.serialize<DragData> dragData)
            |> ignore

    let getData (evt : DragEvent) =
        Browser.Dom.console.log($"getData: {evt.dataTransfer.getData(format)}")
        evt.dataTransfer.getData(format)
            |> Json.parseAs<DragData>

module View =

    module private Type =

        let className (typ : Type) =
            (string typ).ToLower()

    let private renderGoal (goalOpt : Option<Type>) =
        Html.div [
            prop.className "goal-area"
            prop.children [
                match goalOpt with
                    | Some goal ->
                        Html.div [
                            prop.classes [
                                "type"
                                Type.className goal
                            ]
                        ]
                    | None -> ()
            ]
        ]

    module private Term =

        let id (term : Term) =
            $"term-{term.Name}"

    let private renderTerm term goalOpt highlight dispatch =

        let allowTacticExact (evt : DragEvent) =
            Some term.Type = goalOpt
                && (DragData.getData evt).TacticType = TacticType.Exact

        Html.div [

            prop.id (Term.id term)

            prop.classes [
                "term"
                if highlight then "term-highlight"
                Type.className term.Type
            ]

            // https://stackoverflow.com/questions/40940288/drag-datatransfer-data-unavailable-in-ondragover-event

            prop.onDragEnter (fun evt ->
                evt.preventDefault()
                dispatch (HighlightTerm (term.Name, true)))

            prop.onDragOver (fun evt ->
                evt.preventDefault())

            prop.onDragLeave (fun evt ->
                evt.preventDefault()
                dispatch (HighlightTerm (term.Name, false)))

            prop.onDrop (fun evt ->
                evt.preventDefault()
                let msg =
                    if allowTacticExact evt then
                        Audio.playReward ()
                        AddTactic (Exact term)
                    else
                        Audio.playError ()
                        HighlightTerm (term.Name, false)
                dispatch msg)
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

    let private renderTacticTypes (tacticTypes : seq<TacticType>) =
        Html.div [
            prop.className "tactics-area"
            prop.children [
                for tacticType in tacticTypes do
                    Html.div [
                        prop.className "tactic"
                        match tacticType with
                            | TacticType.Exact -> "🎆"
                            | TacticType.Intro -> "🚀"
                            | TacticType.Apply -> "👣"
                            |> prop.text
                        prop.draggable true
                        prop.onDragStart (
                            DragData.setData
                                { TacticType = tacticType })
                    ]
            ]
        ]

    let render (model : Model) (dispatch : Msg -> unit) =
        Html.div [
            renderGoal model.Proof.Goal
            renderTerms model dispatch
            renderTacticTypes model.TacticTypes
        ]
