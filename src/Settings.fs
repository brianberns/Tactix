namespace Tactix

open Browser
open Fable.SimpleJson

/// User settings.
type Settings =
    {
        /// Audio enabled/disabled.
        AudioEnabled : bool

        /// 0-based index of the current level.
        LevelIndex : int
    }

module Settings =

    /// Initial settings.
    let initial =
        {
            AudioEnabled = true
            LevelIndex = 0
        }

    /// Local storage key.
    let private key = "Tactix"

    /// Saves the given settings.
    let save settings =
        WebStorage.localStorage[key]
            <- Json.serialize<Settings> settings

    /// Answers the current settings.
    let get () =
        let json = WebStorage.localStorage[key]
        if isNull json then
            let settings = initial
            save settings
            settings
        else
            Json.parseAs<Settings>(json)

type Settings with

    /// Current settings.
    static member Current =
        Settings.get()
