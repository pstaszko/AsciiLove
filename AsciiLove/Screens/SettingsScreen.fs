namespace AsciiLove.Screens
//Op: Auto
open System

open ConsoleMVU.Contracts
open ConsoleMVU.Runtime

open AsciiLove
//Op: End
module SettingsScreen =
    type SettingsFeatureModel =
        {
            IncrementStep: int
            DecrementStep: int
        }

    let private toFeatureModel (model: Model) : SettingsFeatureModel =
        {
            IncrementStep = model.Settings.IncrementStep
            DecrementStep = model.Settings.DecrementStep
        }

    let private fromFeatureModel (featureModel: SettingsFeatureModel) (model: Model) : Model =
        {
            model with
                Settings =
                    {
                        model.Settings with
                            IncrementStep = featureModel.IncrementStep
                            DecrementStep = featureModel.DecrementStep
                    }
        }

    let private updateFeatureModel (model: SettingsFeatureModel) (msg: SettingsMsg) : FeatureUpdateResult<SettingsFeatureModel, SettingsMsg> =
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
        FeatureAdapter.update
            toFeatureModel
            fromFeatureModel
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