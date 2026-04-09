namespace AsciiLove.Screens

open AsciiLove
open System

module HelpScreen =
    let view (_: Model) =
        Console.SetCursorPosition(0, 0)
        Console_WriteLine("Help Screen")
        Console_WriteLine("this is a dummy screen for example purposes")

    let update (model: Model) (msg: HelpMsg) =
        match msg with
        | CloseHelp ->
            post (goToScreen Main)
            model

    let hotkeys =
        [
            //Op: Auto
            Hotkey.Create("Esc", "Close Help", goToScreen Main)
            //Op: End
        ]