namespace Tactix

open Browser
open Elmish
open Elmish.React

module App =

    // window.alert($"Your screen is {window.innerWidth}x{window.innerHeight} pixels")
    if window.innerWidth < 600
        || window.innerHeight < 600 then
        window.alert("This game works best on a larger screen, but we'll give it a try!")

    Program.mkProgram Model.init Model.update View.render
        |> Program.withReactSynchronous "elmish-app"
        |> Program.run
