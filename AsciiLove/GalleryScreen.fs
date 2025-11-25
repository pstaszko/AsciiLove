namespace AsciiLove.Screens
//Op: Auto
open System
open System.Diagnostics
open System.IO

open AsciiLove.ArtDomain
open AsciiLove.Domain
open AsciiLove.Messages
open Atomic.Operators

open AsciiLove

open Spectre.Console
//Op: End
module GalleryScreen =
    module ColorPrint =
        let printWithColor (color: string) (line: string) =
            let reset = "\u001b[0m"
            Console_WriteLine($"{color}%s{line}{reset}")

    let render (width: int) (file: string) =
        let image = new CanvasImage(file)
        AnsiConsole.Write(image)

    let view (model: Model) =
        Console.SetCursorPosition(0, 0)
        match model.ArtSettings.CurrentArt with
        | None -> ()
        | Some art ->
            let printText() =
                CLEAR "Before printing art in GallaryScreen.view"
                let renderInfo = art.RenderInfo()
                ConsoleUtils.setSize renderInfo.Size
                ColorPrint.printWithColor renderInfo.ColorCode renderInfo.Text
            match model.ArtSettings.ArtDisplayMode, art.Image with
            | ArtDisplayMode.Text, _ -> printText()
            | ArtDisplayMode.ImageIfAvailable, Some image -> render 0 image
            | ArtDisplayMode.ImageIfAvailable, None -> printText()

    let update (model: Model) (msg: GalleryMsg) =
        let openExplorerToFile (path: string) =
            let psi = new ProcessStartInfo("explorer.exe", $"/select,{path}")
            psi.UseShellExecute <- true
            psi
            |> Process.Start
            |> ignore

        let shellExecute (p: string) =
            let psi = new ProcessStartInfo(p)
            psi.UseShellExecute <- true
            psi
            |> Process.Start
            |> ignore

        match msg with
        | CheckForChanges ->
            model.ArtSettings.CurrentArt
            |> Option.map
                ^ fun art ->
                    if art.TxtLastWrite < File.GetLastWriteTime(art.TxtFullPath) then
                        let artWithoutCurrent = model.ArtSettings.AllArt |> List.filter (fun a -> a <> art)
                        let reloadedCurrentArt = pathToArt art.Color art.TxtFullPath

                        { model with ArtSettings = { model.ArtSettings with AllArt = reloadedCurrentArt :: artWithoutCurrent; CurrentArt = Some reloadedCurrentArt } }
                    else
                        model
            |> Option.defaultValue model

        | OpenImageFileIfExists ->
            model.ArtSettings.CurrentArt
            |> Option.bind _.Image
            </> shellExecute
            |> ignore

            model
        | ExploreImageText ->
            model.ArtSettings.CurrentArt
            </> _.TxtFullPath
            </> openExplorerToFile
            |> fun x -> x
            |> ignore

            model
        | OpenImageText ->
            model.ArtSettings.CurrentArt
            </> _.TxtFullPath
            <|> shellExecute

            model
        | SetDisplayMode mode ->
            { model with ArtSettings = { model.ArtSettings with ArtDisplayMode = mode } }
        | SetFilter filter ->
            post (GlobalMsg GlobalMsg.ClearScreen)
            post (GalleryMsg SetRandomArt)
            { model with ArtSettings = { model.ArtSettings with ArtFilter = filter } }
        | Next ->
            let nextArt = model.ArtSettings.NextArt()
            match nextArt with
            | Some art -> { model with ArtSettings = { model.ArtSettings with CurrentArt = Some art } }
            | None     -> model
        | Previous ->
            let previousArt = model.ArtSettings.PreviousArt()
            match previousArt with
            | Some art -> { model with ArtSettings = { model.ArtSettings with CurrentArt = Some art } }
            | None     -> model
        //Op: Auto
        | SetRandomArt -> { model with ArtSettings = model.ArtSettings.SetRandomArt() }
        | ClearArt     -> { model with ArtSettings = { model.ArtSettings with CurrentArt = None } }
        | LoadArt      -> { model with ArtSettings = { model.ArtSettings with AllArt = getAllArt() } }
        //Op: End
        | Show ->
            GalleryMsg LoadArt |> post
            GlobalMsg GlobalMsg.ClearScreen |> post
            GalleryMsg SetRandomArt |> post
            goToScreen Gallery |> post
            model
        | DisplayArt art ->
            let dirty =
                match model.ArtSettings.CurrentArt with
                | Some currentArt when currentArt = art -> false
                | _ -> true
            { model with ArtSettings = { model.ArtSettings with CurrentArt = Some art } }
            |> fun x -> x.SetDirty dirty

    let hotkeys =
        [
            //Op: Auto
            Hotkey.Create("+", "", GalleryMsg Next)
            Hotkey.Create("-", "", GalleryMsg Previous)
            Hotkey.Create("1", "", GalleryMsg (SetDisplayMode ArtDisplayMode.ImageIfAvailable))
            Hotkey.Create("2", "", GalleryMsg (SetDisplayMode ArtDisplayMode.Text))
            Hotkey.Create("3", "", GalleryMsg (SetFilter ArtFilter.All))
            Hotkey.Create("4", "", GalleryMsg (SetFilter ArtFilter.ImageOnly))
            Hotkey.Create("5", "", GalleryMsg (SetFilter ArtFilter.Newest))
            Hotkey.Create("t", "", GalleryMsg OpenImageText)
            Hotkey.Create("e", "", GalleryMsg ExploreImageText)
            Hotkey.Create("i", "", GalleryMsg OpenImageFileIfExists)
            //Op: End
        ]