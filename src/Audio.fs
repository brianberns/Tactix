﻿namespace Tactix

open Browser.Types
open Fable.Core

// https://thesharperdev.com/snippets/fsharp-fable-play-audio/
module Audio =

    [<Global("Audio")>] 
    let private factory : HTMLAudioElementType = jsNative

    /// Plays the given file.
    let private play src =
        let audio = factory.Create()
        audio.src <- src
        audio.volume <- 0.1
        audio.play()

    /// Plays the reward sound.
    let playReward () =
        play "https://neal.fun/infinite-craft/reward.mp3"

    /// Plays the error sound.
    let playError () =
        play "https://neal.fun/infinite-craft/error.mp3"

    /// Plays the discovery sound.
    let playDiscovery () =
        play "https://neal.fun/infinite-craft/discovery.mp3"
