namespace Tactix

open Elmish

// https://www.ma.imperial.ac.uk/~buzzard/lean_together/source/contents.html

type Tactic =
    | Exact

type Model =
    {
        Tactics : List<Tactic>
    }

type Msg = unit

module Model =

    let init () =
        let model =
            {
                Tactics = [ Exact ]
            }
        model, Cmd.none

    let update (msg : Msg) (model : Model) =
        model, Cmd.none
