namespace Tactix

open Browser.Types
open Feliz

/// Data carried during an HTML drag/drop operation.
type private DragData =
    {
        /// Tactic type being dragged.
        TacticType : TacticType
    }

module private DragData =

    // https://stackoverflow.com/questions/40940288/drag-datatransfer-data-unavailable-in-ondragover-event
    // https://stackoverflow.com/questions/31915653/how-to-get-data-from-datatransfer-getdata-in-event-dragover-or-dragenter
    let mutable shared : Option<DragData> = None

    /// Sets drag data for the given event.
    let private setData dragData (evt : DragEvent) =
        // evt.dataTransfer.setData
        shared <- Some dragData

    /// Gets drag data for the given event.
    let private getData (evt : DragEvent) =
        // evt.dataTransfer.getData
        shared

    /// Sets the tactic type being dragged.
    let setTacticType tacticType evt =
        setData { TacticType = tacticType } evt

    /// Gets the tactic type being dragged.
    let getTacticType evt =
        match getData evt with
            | Some dragData -> dragData.TacticType
            | None -> failwith "Unexpected"

module View =

    /// Renders header information.
    let private renderHeader proof levelIdx =
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

    /// Renders the given tactic type as an emoji.
    let private renderTacticType tacticType draggable =
        Html.div [
            prop.className "tactic"
            prop.text (TacticType.emoji tacticType)
            prop.title (TacticType.instructions tacticType)
            if draggable then
                prop.draggable true
                prop.onDragStart (
                    DragData.setTacticType tacticType)
        ]

    /// Renders tactics that apply to goals.
    let private renderGoalTactics levelIdx draggable =
        let tacticTypes =
            Level.levels[levelIdx].GoalTactics
        Html.div [
            prop.id "goal-tactics"
            prop.children [
                for tacticType in tacticTypes do
                    renderTacticType tacticType draggable
            ]
        ]

    /// Renders the given proposition.
    let private renderProp prp =

        /// Renders the given propositions with the given separator
        /// between them.
        let between content f props =
            [
                for (i, prp) in Seq.indexed props do
                    if i > 0 then
                        yield Html.span [
                            prop.innerHtml content ]
                    yield f prp
            ]

        let rec loop prp =
            Html.div [
                match prp with
                    | Primitive name ->
                        prop.classes [
                            "primitive-prop"
                            name.ToLower()
                        ]
                    | Impliction (p, q) ->
                        prop.className "compound-prop"
                        between Text.implies loop [p; q]
                            |> prop.children
                    | Conjunction props ->
                        prop.className "compound-prop"
                        between Text.andSymbol loop props
                            |> prop.children
                    | Disjunction props ->
                        prop.className "compound-prop"
                        between Text.orSymbol loop props
                            |> prop.children
                    | Not inner ->
                        prop.className "compound-prop"
                        prop.children [
                            Html.span [
                                prop.innerHtml Text.notSymbol ]
                            loop inner
                        ]
            ]

        loop prp

    /// Renders the given natural number.
    let private renderNum num =

        let rec loop num =
            Html.div [
                match num with
                    | Zero ->
                        prop.classes [
                            "primitive-prop"
                            "p"
                        ]
                    | Successor n ->
                        prop.className "compound-prop"
                        prop.children [ loop n ]
            ]

        loop num

    /// Renders the given type.
    let private renderType = function
        | Proposition prop -> renderProp prop
        | NaturalNumber num -> renderNum num

    /// Renders drag/drop properties.
    let private renderDragDrop
        highlightMsg
        allow
        audioEnabled
        dispatch =

        [
            if highlightMsg <> Message.noHighlight then

                    // start highlighting the target?
                prop.onDragEnter (fun evt ->
                    if allow evt |> Option.isSome then
                        evt.preventDefault()
                        dispatch highlightMsg)

                    // stop highlighting the target?
                prop.onDragLeave (fun evt ->
                    if allow evt |> Option.isSome then
                        evt.preventDefault()
                        dispatch Message.noHighlight)

                // allow drop?
            prop.onDragOver (fun evt ->
                if allow evt |> Option.isSome then
                    evt.preventDefault())

                // drop has occurred
            prop.onDrop (fun evt ->
                evt.preventDefault()
                match allow evt with
                    | Some msg ->
                        if audioEnabled then Audio.playReward ()
                        msg
                    | None ->   // should never actually happen
                        if audioEnabled then Audio.playError ()
                        Message.noHighlight
                    |> dispatch)
        ]

    /// Renders the given goal.
    let private renderGoal
        goal
        caseKey
        allow
        model
        dispatch =

            // child HTML elements
        let children = renderType goal

            // drag/drop properties
        let dragDrop =
            let highlightMsg =
                Message.highlightGoal goal caseKey
            renderDragDrop
                highlightMsg
                allow
                model.Settings.AudioEnabled
                dispatch

            // goal is highlighted?
        let isHighlighted =
            model.IsHighlighted(goal, caseKey)

            // goal is primitive?
        let isPrimitive = Type.isPrimitive goal

        Html.div [
            match isHighlighted, isPrimitive with
                | true, true -> "primitive-prop-highlight"
                | true, false -> "compound-prop-highlight"
                | false, _ -> "prop"
                |> prop.className
            prop.children children
            yield! dragDrop
        ]

    /// Renders goals for the given proof case.
    let private renderGoals
        ((caseKey, case) as casePair)
        model
        dispatch =
        assert(model.Proof.CaseMap[caseKey] = case)

            // enable goal-level tactics
        let allowMulti (goal : Type) evt =
            let tacticType = DragData.getTacticType evt
            match goal with
                | Proposition prp ->
                    Allow.any [
                        Allow.allow Intro casePair
                        Allow.allow DissolveGoal casePair
                        Allow.allow SplitGoal casePair
                        Allow.allow AffirmGoal casePair
                    ] prp tacticType
                | NaturalNumber num ->
                    None

        Html.div [
            prop.className "goals"
            prop.children [

                    // complete case rendered specially
                if case.IsComplete then
                    Html.div [
                        prop.className "complete"
                        prop.text (TacticType.emoji TacticType.Exact)
                    ]

                    // render each goal
                else
                    for goal in case.Goals do
                        renderGoal
                            goal
                            caseKey
                            (allowMulti goal)
                            model
                            dispatch
            ]
        ]

    /// Renders the given term.
    let private renderTerm
        term
        caseKey
        allow
        model
        dispatch =

            // child HTML elements
        let children = renderType term.Type

            // drag/drop properties
        let dragDrop =
            let highlightMsg =
                Message.highlightTerm term caseKey
            renderDragDrop
                highlightMsg
                allow
                model.Settings.AudioEnabled
                dispatch

            // term is highlighted?
        let isHighlighted =
            model.IsHighlighted(term, caseKey)

            // term is primitive?
        let isPrimitive = Type.isPrimitive term.Type

        Html.div [
            match isHighlighted, isPrimitive with
                | true, true -> "primitive-term-highlight"
                | true, false -> "compound-term-highlight"
                | false, _ -> "term"
                |> prop.className
            prop.children children
            yield! dragDrop
        ]

    /// Renders terms for the given proof case.
    let private renderTerms
        ((caseKey, case) as casePair)
        model
        dispatch =
        assert(model.Proof.CaseMap[caseKey] = case)

            // enable term-level tactics
        let allowMulti term evt =
            let tacticType = DragData.getTacticType evt
            Allow.any [
                Allow.allow Exact casePair
                Allow.allow Apply casePair
                Allow.allow SplitTerm casePair
                Allow.allow DissolveTerm casePair
                Allow.allow AffirmTerm casePair
            ] term tacticType

            // render each term
        Html.div [
            prop.className "terms"
            prop.children [
                for term in case.Terms do
                    renderTerm
                        term
                        caseKey
                        (allowMulti term)
                        model
                        dispatch
            ]
        ]

    /// Renders the given proof case.
    let private renderProofCase casePair model dispatch =
        Html.div [
            prop.className "proof-case"
            prop.children [
                renderGoals casePair model dispatch
                renderTerms casePair model dispatch
            ]
        ]

    /// Renders the given proof.
    let private renderProof model dispatch =
        Html.div [
            prop.id "proof"
            prop.children [
                for casePair in Map.toSeq model.Proof.CaseMap do
                    renderProofCase casePair model dispatch
            ]
        ]

    /// Renders tactics that apply to terms.
    let private renderTermTactics levelIdx draggable =
        let tacticTypes =
            Level.levels[levelIdx].TermTactics
        Html.div [
            prop.id "term-tactics"
            prop.children [
                for tacticType in tacticTypes do
                    renderTacticType tacticType draggable
            ]
        ]

    /// Renders footer information.
    let private renderFooter settings dispatch =
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

    /// Renders the given model.
    let render model dispatch =
        Html.div [

                // child HTML elements
            let levelIdx = model.Settings.LevelIndex
            let isActive =
                not <| Proof.isComplete model.Proof
            prop.children [
                renderHeader model.Proof levelIdx
                renderGoalTactics levelIdx isActive
                renderProof model dispatch
                renderTermTactics levelIdx isActive
                renderFooter
                    model.Settings
                    dispatch
            ]
                // easter egg for revisiting levels
            prop.onCut (fun _ ->
                let levelIdx =
                    if levelIdx > 0 then levelIdx - 1
                    else Level.levels.Length - 1
                dispatch (StartLevel levelIdx))
        ]
