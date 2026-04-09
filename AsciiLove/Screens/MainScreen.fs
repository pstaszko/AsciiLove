namespace AsciiLove.Screens
//Op: Auto
open System

open ConsoleMVU.Contracts
open ConsoleMVU.Runtime

open AsciiLove
//Op: End
module MainScreen =
    type MainFeatureModel =
        {
            Count: int
            Busy: bool
            IncrementStep: int
            DecrementStep: int
        }

    let private toFeatureModel (model: Model) : MainFeatureModel =
        {
            Count = model.Count
            Busy = model.Busy
            IncrementStep = model.Settings.IncrementStep
            DecrementStep = model.Settings.DecrementStep
        }

    let private fromFeatureModel (featureModel: MainFeatureModel) (model: Model) : Model =
        {
            model with
                Count = featureModel.Count
                Busy = featureModel.Busy
                Settings =
                    {
                        model.Settings with
                            IncrementStep = featureModel.IncrementStep
                            DecrementStep = featureModel.DecrementStep
                    }
        }

    let private updateFeatureModel (model: MainFeatureModel) (msg: MainMsg) : FeatureUpdateResult<MainFeatureModel, MainMsg> =
        match msg with
        | Increment ->
            {
                Model = { model with Count = model.Count + model.IncrementStep }
                Effects = []
            }
        | Decrement ->
            {
                Model = { model with Count = model.Count - model.DecrementStep }
                Effects = []
            }
        | DelayedIncrement ->
            {
                Model = { model with Busy = true }
                Effects = [ FeatureEffect.DelayDispatchMs(1000, DelayedIncrementCompleted) ]
            }
        | DelayedIncrementCompleted ->
            {
                Model =
                    {
                        model with
                            Busy = false
                            Count = model.Count + model.IncrementStep
                    }
                Effects = []
            }

    let view (model: Model) =
        Console.SetCursorPosition(0, 0)
        Console_WriteLine($"Count: %i{model.Count}      ")
        if model.Busy then Console_WriteLine("Working...        ")
        else Console_WriteLine("                  ")
        Console_WriteLine($"Increment step: %i{model.Settings.IncrementStep}  Decrement step: %i{model.Settings.DecrementStep}")

    let update (model: Model) (msg: MainMsg) : Model =
        let featureModel = toFeatureModel model
        let result = updateFeatureModel featureModel msg

        result.Effects
        |> EffectRunner.run (MainMsg >> post)

        fromFeatureModel result.Model model

    let hotkeys =
        [
            //Op: Auto
            Hotkey.Create("+", "Increment", MainMsg Increment)
            Hotkey.Create("-", "Decrement", MainMsg Decrement)
            Hotkey.Create("d", "Delay +"  , MainMsg DelayedIncrement)
            //Op: End
        ]