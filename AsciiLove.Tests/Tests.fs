namespace AsciiLove.Tests
//Op: Auto
open System

open AsciiLove.Domain
open AsciiLove.Main
open Atomic.Operators

open AsciiLove

open FsUnit.Xunit
open Xunit
//Op: End
[<AutoOpen>]
module Utils =
    let winmergeEqual expected actual =
        let srl x = Newtonsoft.Json.JsonConvert.SerializeObject(x, Newtonsoft.Json.Formatting.Indented)
        WinMergeEquals.WinMergeEquals.AreEqualWinMerge (srl expected) (srl actual) WinMergeEquals.WhitespaceSimplify.None "Exp" "Act"

type ``MVU Tests``() =
    let SetDirtyTrue (model: Model) = model.SetDirty true
    let stepAdapt model (maybeChar: char option) =
        let maybeConsoleKeyInfo = maybeChar |> Option.map ^ fun a -> new ConsoleKeyInfo(a, ConsoleKey.Escape, false, false, false)
        Program.step (fun _ _ -> ()) maybeConsoleKeyInfo model
    [<Fact>]
    member _.``No messages, no changes``() =
        let init = AsciiLove.Main.init
        stepAdapt init None
        |> should equal (init, LoopBehavior.Continue)

    [<Fact>]
    member _.``Clear screen, no changes to model``() =
        let init = AsciiLove.Main.init |> SetDirtyTrue
        post (GlobalMsg ClearScreen)
        stepAdapt init None
        |> should equal (init, LoopBehavior.Continue)

    [<Fact>]
    member _.``Go settings, changes``() =
        let init = AsciiLove.Main.init
        post (goToScreen Settings)
        stepAdapt init None
        |> should equal ({ init with Screen = Settings }.SetDirty true, LoopBehavior.Continue)

    [<Fact>]
    member _.``Sending Q, quits``() =
        let init = AsciiLove.Main.init
        stepAdapt init (Some 'q')
        |> should equal (init, LoopBehavior.Stop)

    [<Fact>]
    member _.``Sending s, go to settings``() =
        let init = AsciiLove.Main.init
        stepAdapt init (Some 's')
        |> should equal ({ init with Screen = Settings }.SetDirty true, LoopBehavior.Continue)

    [<Fact>]
    member _.``Sending s from help, go to settings``() =
        let init = AsciiLove.Main.init
        let helpScreen = stepAdapt init (Some 'h') |> fst
        stepAdapt helpScreen (Some 's')
        |> should equal ({ init with Screen = Settings }.SetDirty true, LoopBehavior.Continue)

    [<Fact>]
    member _.``Sending + from settings - bumps up increment``() =
        let init = AsciiLove.Main.init
        let settingsScreen = stepAdapt init (Some 's') |> fst
        stepAdapt settingsScreen (Some '+')
        |> should equal ({ init with Screen = Settings; Settings = {init.Settings with IncrementStep = 2 } }.SetDirty true, LoopBehavior.Continue)

    [<Fact>]
    member _.``Sending + from settings - bumps up increment - has effect on main``() =

        let init = AsciiLove.Main.init
        let settingsScreen = stepAdapt init (Some 's') |> fst
        stepAdapt settingsScreen (Some '+')
        |> fst
        |> (swap stepAdapt) (Some 'm')
        |> fst
        |> (swap stepAdapt) (Some '+')
        |> winmergeEqual ({ init with Screen = Main; Count = 2; Settings = {init.Settings with IncrementStep = 2 } }.SetDirty true, LoopBehavior.Continue)

    [<Fact>]
    member _.``Sending m from settings - goes to main``() =
        let init = AsciiLove.Main.init
        let settingsScreen = stepAdapt init (Some 's') |> fst
        stepAdapt settingsScreen (Some 'm')
        |> should equal ({ init with Screen = Main }.SetDirty true, LoopBehavior.Continue)