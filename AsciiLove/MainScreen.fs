namespace AsciiLove.Screens
//Op: Auto
open System
open System.Threading.Tasks

open AsciiLove.Domain
open AsciiLove.Messages
open Atomic.Operators

open AsciiLove
//Op: End
module MainScreen =

    let view (model: Model) =
        Console.SetCursorPosition(0, 0)
        Console_WriteLine($"Count: %i{model.Count}      ")
        if model.Busy then Console_WriteLine("Working...        ")
        else Console_WriteLine("                  ")
        Console_WriteLine($"Increment step: %i{model.Settings.IncrementStep}  Decrement step: %i{model.Settings.DecrementStep}")

    let update (model: Model) (msg: MainMsg) =
        match msg with
        | Increment ->
            { model with Count = model.Count + model.Settings.IncrementStep }
        | Decrement ->
            { model with Count = model.Count - model.Settings.DecrementStep }
        | DelayedIncrement ->
            Task.Delay(1000)
                .ContinueWith(fun _ -> post (MainMsg DelayedIncrementCompleted))
            |> ignore

            { model with Busy = true }
        | DelayedIncrementCompleted ->
            { model with Busy = false; Count = model.Count + model.Settings.IncrementStep }

    let hotkeys =
        [
            //Op: Auto
            Hotkey.Create("+", "Increment", MainMsg Increment)
            Hotkey.Create("-", "Decrement", MainMsg Decrement)
            Hotkey.Create("d", "Delay +"  , MainMsg DelayedIncrement)
            //Op: End
        ]