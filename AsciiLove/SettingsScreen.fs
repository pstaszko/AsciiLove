namespace AsciiLove.Screens
//Op: Auto
open System

open AsciiLove.Domain
open Atomic.Operators

open AsciiLove
//Op: End
module SettingsScreen =
    let view (model: Model) =
        Console.SetCursorPosition(0, 0)
        Console_WriteLine("Settings")
        Console_WriteLine($"Increment step: %i{model.Settings.IncrementStep} (+/-)    ")
        Console_WriteLine($"Decrement step: %i{model.Settings.DecrementStep} ([ / ])    ")

    let update (model: Model) (msg: SettingsMsg) =
        match msg with
        | IncIncrementStep -> { model with Settings = { model.Settings with IncrementStep = model.Settings.IncrementStep + 1 } }
        | DecIncrementStep -> { model with Settings = { model.Settings with IncrementStep = model.Settings.IncrementStep - 1 } }
        | IncDecrementStep -> { model with Settings = { model.Settings with DecrementStep = model.Settings.DecrementStep + 1 } }
        | DecDecrementStep -> { model with Settings = { model.Settings with DecrementStep = model.Settings.DecrementStep - 1 } }

    let hotkeys =
        [
            //Op: Auto
            Hotkey.Create("+", "Increment Step"          , SettingsMsg IncIncrementStep)
            Hotkey.Create("-", "Decrement Step"          , SettingsMsg DecIncrementStep)
            Hotkey.Create("[", "Decrement Decrement Step", SettingsMsg DecDecrementStep)
            Hotkey.Create("]", "Increment Decrement Step", SettingsMsg IncDecrementStep)
            //Op: End
        ]