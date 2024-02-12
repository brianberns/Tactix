module App

open Elmish
open Elmish.React
open Fable.Core.JsInterop
open Fable.SimpleJson
open Feliz

type Model =
    | Left of float * float
    | Right of float * float

    member this.IsLeft_ =
        match this with
            | Left _ -> true
            | Right _ -> false

    member this.IsRight_ =
        match this with
            | Left _ -> false
            | Right _ -> true

type Msg =
    | MoveLeft of float * float
    | MoveRight of float * float

let init () = Left (10.0, 10.0), Cmd.none

let update (msg : Msg) (model : Model) =
    match msg, model with
        | MoveLeft (x, y), Right _ ->
            Left (x, y), Cmd.none
        | MoveRight (x, y), Left _ ->
            Right (x, y), Cmd.none
        | _ -> failwith "Unexpected"

// https://github.com/facebook/react/issues/4431
type Browser.Types.DragEvent with
    member evt.OffsetX : float = evt?nativeEvent?offsetX
    member evt.OffsetY : float = evt?nativeEvent?offsetY

let format = "application/json"

let render (model : Model) (dispatch : Msg -> unit) =

    let dragMe (x : float, y : float) =
        Html.p [
            prop.id "drag-me"
            prop.text "Drag Me"
            prop.draggable true
            prop.style [
                style.left (length.px x)
                style.top (length.px y)
            ]
            prop.onDragStart (fun evt ->
                let x : float = evt.OffsetX
                let y : float = evt.OffsetY
                evt.dataTransfer.setData(
                    format, Json.serialize (x, y))
                    |> ignore)
        ]

    let targetProps move =
        [
            prop.onDragOver (fun evt ->
                evt.preventDefault())
            prop.onDrop (fun evt ->
                evt.preventDefault()
                let xStart, yStart =
                    evt.dataTransfer.getData(format)
                        |> Json.parseAs<float * float>
                let x = evt.OffsetX - xStart
                let y = evt.OffsetY - yStart
                dispatch <| move (x, y))
        ]

    Html.div [
        Html.div [
            prop.id "left"
            match model with
                | Left (x, y) ->
                    prop.children [ dragMe (x, y) ]
                | Right _ ->
                    yield! targetProps MoveLeft
        ]
        Html.div [
            prop.id "right"
            match model with
                | Left _ ->
                    yield! targetProps MoveRight
                | Right (x, y) ->
                    prop.children [ dragMe (x, y) ]
        ]
    ]

Program.mkProgram init update render
    |> Program.withReactSynchronous "elmish-app"
    |> Program.run
