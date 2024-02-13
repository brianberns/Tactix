namespace Tactix

open Browser.Types
open Fable.Core

// https://thesharperdev.com/snippets/fsharp-fable-play-audio/
module Audio =

    let [<Global("Audio")>] private factory : HTMLAudioElementType = jsNative

    let private play src =
        let audio = factory.Create()
        audio.src <- src
        audio.volume <- 0.5
        audio.play()

    let playReward () =
        play "https://neal.fun/infinite-craft/reward.mp3"

    let playError () =
        play "https://neal.fun/infinite-craft/error.mp3"
