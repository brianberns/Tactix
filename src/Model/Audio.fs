namespace Tactix

open Browser.Types
open Fable.Core

// https://thesharperdev.com/snippets/fsharp-fable-play-audio/
module Audio =

    [<Global("Audio")>] 
    let private factory : HTMLAudioElementType = jsNative

    /// Creates an audio element for the given file.
    let private create src =
        let audio = factory.Create()
        audio.src <- src
        audio

    let private reward = lazy create "reward.mp3"
    let private error = lazy create "error.mp3"
    let private discovery = lazy create "discovery.mp3"

    /// Enables audio. This must be done during a user interaction
    /// for mobile devices.
    let enable () =
        reward.Value |> ignore
        error.Value |> ignore
        discovery.Value |> ignore

    /// Plays a lazy audio element.
    let private play (audio : Lazy<HTMLAudioElement>) =
        audio.Value.play()

    /// Plays the reward sound.
    let playReward () =
        play reward

    /// Plays the error sound.
    let playError () =
        play error

    /// Plays the discovery sound.
    let playDiscovery () =
        play discovery
