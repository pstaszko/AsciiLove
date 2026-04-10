namespace AsciiLove.Screens
//Op: Auto
open System

open ConsoleMVU.Contracts
open ConsoleMVU.Runtime

open AsciiLove
//Op: End
module SettingsScreen =
    let private settingsSlice =
        FeatureMapping.slice
            (fun (model: Model) -> model.Settings)
            (fun settings (model: Model) -> { model with Settings = settings })

    let private updateFeatureModel (model: SavedSettings) (msg: SettingsMsg) : FeatureUpdateResult<SavedSettings, SettingsMsg> =
        let updatedModel =
            match msg with
            | IncIncrementStep ->
                { model with IncrementStep = model.IncrementStep + 1 }
            | DecIncrementStep ->
                { model with IncrementStep = model.IncrementStep - 1 }
            | IncDecrementStep ->
                { model with DecrementStep = model.DecrementStep + 1 }
            | DecDecrementStep ->
                { model with DecrementStep = model.DecrementStep - 1 }

        {
            Model = updatedModel
            Effects = []
        }

    let view (model: Model) =
        Console.SetCursorPosition(0, 0)
        Console_WriteLine("Settings")
        Console_WriteLine($"Increment step: %i{model.Settings.IncrementStep} (+/-)    ")
        Console_WriteLine($"Decrement step: %i{model.Settings.DecrementStep} ([ / ])    ")

    let update (model: Model) (msg: SettingsMsg) =
        FeatureMapping.updateSlice
            settingsSlice
            updateFeatureModel
            (fun _ -> ())
            model
            msg

    let hotkeys =
        [
            //Op: Auto
            Hotkey.Create("+", "Increment Step"          , SettingsMsg IncIncrementStep)
            Hotkey.Create("-", "Decrement Step"          , SettingsMsg DecIncrementStep)
            Hotkey.Create("[", "Decrement Decrement Step", SettingsMsg DecDecrementStep)
            Hotkey.Create("]", "Increment Decrement Step", SettingsMsg IncDecrementStep)
            //Op: End
        ]