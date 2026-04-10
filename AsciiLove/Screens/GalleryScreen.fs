namespace AsciiLove.Screens
//Op: Auto
open System
open System.Diagnostics

open ConsoleMVU.Gallery.GalleryMessages
open ConsoleMVU.Gallery
open ConsoleMVU.Gallery.Core
open AsciiLove.Domain
open AsciiLove.Messages
open ConsoleMVU.Runtime

open AsciiLove
//Op: End
module GalleryScreen =
    let private toFeatureModel (model: Model) : GalleryState =
        {
            ArtSettings = model.ArtSettings
            DirtyScreen = model.Window.DirtyScreen
        }

    let private fromFeatureModel (featureModel: GalleryState) (model: Model) : Model =
        { model with ArtSettings = featureModel.ArtSettings }
        |> fun x -> x.SetDirty featureModel.DirtyScreen

    let private runEffect (effect: GalleryEffect) =
        let openExplorerToFile (path: string) =
            let psi = new ProcessStartInfo("explorer.exe", $"/select,{path}")
            psi.UseShellExecute <- true
            psi |> Process.Start |> ignore

        let shellExecute (path: string) =
            let psi = new ProcessStartInfo(path)
            psi.UseShellExecute <- true
            psi |> Process.Start |> ignore

        match effect with
        | GalleryEffect.ClearScreen -> post (GlobalMsg ClearScreen)
        | GalleryEffect.QueueGalleryMessage message -> post (GalleryMsg message)
        | GalleryEffect.OpenGalleryScreen -> post (goToScreen Gallery)
        | GalleryEffect.OpenImageText path -> shellExecute path
        | GalleryEffect.ExploreImageText path -> openExplorerToFile path
        | GalleryEffect.OpenImageFileIfExists path -> shellExecute path

    let view (model: Model) =
        ConsoleMVU.Gallery.GalleryFeature.view CLEAR ConsoleUtils.setSize model.ArtSettings

    let update (model: Model) (msg: ConsoleMVU.Gallery.GalleryMessages.GalleryMsg) =
        FeatureAdapter.update
            toFeatureModel
            fromFeatureModel
            ConsoleMVU.Gallery.GalleryFeature.update
            runEffect
            model
            msg

    let hotkeys =
        ConsoleMVU.Gallery.GalleryFeature.hotkeys
        |> List.map (fun x -> Hotkey.Create(x.Key, x.Description, GalleryMsg x.Message))