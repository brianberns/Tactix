namespace Tactix

open Elmish

type Model =
    {
        LevelIndex : int
        Proof : Proof
        Highlighted : Choice<unit, Term, Type>
        AudioEnabled : bool
    }

type Msg =
    | Highlight of Choice<unit, Term, Type>
    | AddTactic of Tactic
    | EnableAudio of bool
    | StartLevel of int

module Model =

    let init () =
        let levelIdx = 0
        let level = Level.levels[levelIdx]
        let proof = Level.initializeProof level
        let model =
            {
                LevelIndex = levelIdx
                Proof = proof
                Highlighted = Choice1Of3 ()
                AudioEnabled = true
            }
        model, Cmd.none

    let private updateHighlight choice model =
        assert(model.Highlighted <> choice)
        { model with Highlighted = choice }

    let private updateAddTactic tactic model =
        { model with
            Proof = Proof.add tactic model.Proof }

    let private updateEnableAudio enable model =
        assert(model.AudioEnabled <> enable)
        { model with AudioEnabled = enable }

    let private updateStartLevel levelIdx model =
        let proof = Level.initializeProof Level.levels[levelIdx]
        { model with
            LevelIndex = levelIdx
            Proof = proof
            Highlighted = Choice1Of3 () }

    let update (msg : Msg) (model : Model) =
        let model' =
            match msg with
                | Highlight choice ->
                    updateHighlight choice model
                | AddTactic tactic ->
                    updateAddTactic tactic model
                | EnableAudio enable ->
                    updateEnableAudio enable model
                | StartLevel levelIdx ->
                    updateStartLevel levelIdx model
        let cmd =
            if model'.Proof.GoalOpt.IsNone then
                Cmd.OfAsync.perform
                    (fun () -> Async.Sleep 800)
                    ()
                    (fun () ->
                        if model.AudioEnabled then
                            Audio.playDiscovery ()   // to-do: move this side-effect into the view
                        StartLevel (model'.LevelIndex + 1))
            else
                Cmd.none
        model', cmd
