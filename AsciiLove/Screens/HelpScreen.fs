namespace AsciiLove.Screens

open AsciiLove
open System
open ConsoleMVU.Contracts
open ConsoleMVU.Runtime

module HelpScreen =
    type HelpFeatureModel = unit

    let private updateFeatureModel (_: HelpFeatureModel) (msg: HelpMsg) : FeatureUpdateResult<HelpFeatureModel, Msg> =
        match msg with
        | CloseHelp ->
            {
                Model = ()
                Effects = [ FeatureEffect.Dispatch(goToScreen Main) ]
            }

    let view (_: Model) =
        Console.SetCursorPosition(0, 0)
        Console_WriteLine("Help Screen")
        Console_WriteLine("this is a dummy screen for example purposes")

    let update (model: Model) (msg: HelpMsg) =
        FeatureMapping.updateStateless
            updateFeatureModel
            post
            model
            msg

    let hotkeys =
        [
            //Op: Auto
            Hotkey.Create("Esc", "Close Help", goToScreen Main)
            //Op: End
        ]