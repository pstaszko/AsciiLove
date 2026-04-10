namespace AsciiLove.Screens
//Op: Auto
open System
open System.Diagnostics
open System.IO

open AsciiLove.ArtDomain
open AsciiLove.Domain
open AsciiLove.Messages
open Atomic.Operators
open ConsoleMVU.Contracts
open ConsoleMVU.Contracts.FeatureUpdateResult
open ConsoleMVU.Runtime

open AsciiLove

open Spectre.Console
//Op: End
module GalleryScreen =
    type GalleryFeatureModel =
        {
            ArtSettings: ArtSettings
            DirtyScreen: bool
        }

    let private toFeatureModel (model: Model) : GalleryFeatureModel =
        {
            ArtSettings = model.ArtSettings
            DirtyScreen = model.Window.DirtyScreen
        }

    let private fromFeatureModel (featureModel: GalleryFeatureModel) (model: Model) : Model =
        { model with ArtSettings = featureModel.ArtSettings }
        |> fun x -> x.SetDirty featureModel.DirtyScreen

    let private updateFeatureModel (model: GalleryFeatureModel) (msg: GalleryMsg) : FeatureUpdateResult<GalleryFeatureModel, Msg> =

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

                        {
                            model with
                                ArtSettings =
                                    {
                                        model.ArtSettings with
                                            AllArt = reloadedCurrentArt :: artWithoutCurrent
                                            CurrentArt = Some reloadedCurrentArt
                                    }
                        }
                    else
                        model
            |> Option.defaultValue model
            |> noEffects

        | OpenImageFileIfExists ->
            model.ArtSettings.CurrentArt
            |> Option.bind _.Image
            <|> shellExecute

            noEffects model
        | ExploreImageText ->
            model.ArtSettings.CurrentArt
            </> _.TxtFullPath
            </> openExplorerToFile
            |> ignore

            noEffects model
        | OpenImageText ->
            model.ArtSettings.CurrentArt
            </> _.TxtFullPath
            <|> shellExecute

            noEffects model
        | SetDisplayMode mode ->
            {
                model with
                    ArtSettings =
                        {
                            model.ArtSettings with
                                ArtDisplayMode = mode
                        }
            }
            |> noEffects
        | SetFilter filter ->
            {
                Model =
                    {
                        model with
                            ArtSettings =
                                {
                                    model.ArtSettings with
                                        ArtFilter = filter
                                }
                    }
                Effects =
                    [
                        FeatureEffect.Dispatch(GlobalMsg GlobalMsg.ClearScreen)
                        FeatureEffect.Dispatch(GalleryMsg SetRandomArt)
                    ]
            }
        | Next ->
            let nextArt = model.ArtSettings.NextArt()
            match nextArt with
            | Some art ->
                {
                    model with
                        ArtSettings =
                            {
                                model.ArtSettings with
                                    CurrentArt = Some art
                            }
                }
                |> noEffects
            | None ->
                noEffects model
        | Previous ->
            let previousArt = model.ArtSettings.PreviousArt()
            match previousArt with
            | Some art ->
                {
                    model with
                        ArtSettings =
                            {
                                model.ArtSettings with
                                    CurrentArt = Some art
                            }
                }
                |> noEffects
            | None ->
                noEffects model
        //Op: Auto
        | SetRandomArt -> { model with ArtSettings = model.ArtSettings.SetRandomArt() } |> noEffects
        | ClearArt     -> { model with ArtSettings = { model.ArtSettings with CurrentArt = None } } |> noEffects
        | LoadArt      -> { model with ArtSettings = { model.ArtSettings with AllArt = getAllArt() } } |> noEffects
        //Op: End
        | Show ->
            {
                Model = model
                Effects =
                    [
                        FeatureEffect.Dispatch(GalleryMsg LoadArt)
                        FeatureEffect.Dispatch(GlobalMsg GlobalMsg.ClearScreen)
                        FeatureEffect.Dispatch(GalleryMsg SetRandomArt)
                        FeatureEffect.Dispatch(goToScreen Gallery)
                    ]
            }
        | DisplayArt art ->
            let dirty =
                match model.ArtSettings.CurrentArt with
                | Some currentArt when currentArt = art -> false
                | _ -> true

            {
                Model =
                    {
                        model with
                            ArtSettings =
                                {
                                    model.ArtSettings with
                                        CurrentArt = Some art
                                }
                            DirtyScreen = dirty
                    }
                Effects = []
            }

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
        FeatureAdapter.update
            toFeatureModel
            fromFeatureModel
            updateFeatureModel
            post
            model
            msg

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