namespace ConsoleMVU.Gallery.Core

open System.IO
open ConsoleMVU.Contracts
open ConsoleMVU.Gallery.GalleryDomain
open ConsoleMVU.Gallery.GalleryMessages

type GalleryState =
    {
        ArtSettings: ArtSettings
        DirtyScreen: bool
    }

[<RequireQualifiedAccess>]
type GalleryEffect =
    | ClearScreen
    | QueueGalleryMessage of GalleryMsg
    | OpenGalleryScreen
    | OpenImageText of string
    | ExploreImageText of string
    | OpenImageFileIfExists of string

module GalleryFeature =
    let update (state: GalleryState) (msg: GalleryMsg) : FeatureUpdateResult<GalleryState, GalleryEffect> =
        let noEffects model =
            {
                Model = model
                Effects = []
            }

        match msg with
        | CheckForChanges ->
            state.ArtSettings.CurrentArt
            |> Option.map (fun art ->
                if art.TxtLastWrite < File.GetLastWriteTime(art.TxtFullPath) then
                    let artWithoutCurrent = state.ArtSettings.AllArt |> List.filter (fun a -> a <> art)
                    let reloadedCurrentArt = pathToArt art.Color art.TxtFullPath

                    {
                        state with
                            ArtSettings =
                                {
                                    state.ArtSettings with
                                        AllArt = reloadedCurrentArt :: artWithoutCurrent
                                        CurrentArt = Some reloadedCurrentArt
                                }
                    }
                else
                    state)
            |> Option.defaultValue state
            |> noEffects
        | OpenImageFileIfExists ->
            let effects =
                state.ArtSettings.CurrentArt
                |> Option.bind _.Image
                |> Option.map (GalleryEffect.OpenImageFileIfExists >> FeatureEffect.Dispatch)
                |> Option.toList

            {
                Model = state
                Effects = effects
            }
        | ExploreImageText ->
            let effects =
                state.ArtSettings.CurrentArt
                |> Option.map _.TxtFullPath
                |> Option.map (GalleryEffect.ExploreImageText >> FeatureEffect.Dispatch)
                |> Option.toList

            {
                Model = state
                Effects = effects
            }
        | OpenImageText ->
            let effects =
                state.ArtSettings.CurrentArt
                |> Option.map _.TxtFullPath
                |> Option.map (GalleryEffect.OpenImageText >> FeatureEffect.Dispatch)
                |> Option.toList

            {
                Model = state
                Effects = effects
            }
        | SetDisplayMode mode ->
            {
                state with
                    ArtSettings =
                        {
                            state.ArtSettings with
                                ArtDisplayMode = mode
                        }
            }
            |> noEffects
        | SetFilter filter ->
            {
                Model =
                    {
                        state with
                            ArtSettings =
                                {
                                    state.ArtSettings with
                                        ArtFilter = filter
                                }
                    }
                Effects =
                    [
                        FeatureEffect.Dispatch GalleryEffect.ClearScreen
                        FeatureEffect.Dispatch (GalleryEffect.QueueGalleryMessage SetRandomArt)
                    ]
            }
        | Next ->
            let nextArt = state.ArtSettings.NextArt()
            match nextArt with
            | Some art ->
                {
                    state with
                        ArtSettings =
                            {
                                state.ArtSettings with
                                    CurrentArt = Some art
                            }
                }
                |> noEffects
            | None ->
                noEffects state
        | Previous ->
            let previousArt = state.ArtSettings.PreviousArt()
            match previousArt with
            | Some art ->
                {
                    state with
                        ArtSettings =
                            {
                                state.ArtSettings with
                                    CurrentArt = Some art
                            }
                }
                |> noEffects
            | None ->
                noEffects state
        | SetRandomArt ->
            { state with ArtSettings = state.ArtSettings.SetRandomArt() }
            |> noEffects
        | ClearArt ->
            {
                state with
                    ArtSettings =
                        {
                            state.ArtSettings with
                                CurrentArt = None
                        }
            }
            |> noEffects
        | LoadArt ->
            {
                state with
                    ArtSettings =
                        {
                            state.ArtSettings with
                                AllArt = getAllArt()
                        }
            }
            |> noEffects
        | Show ->
            {
                Model = state
                Effects =
                    [
                        FeatureEffect.Dispatch (GalleryEffect.QueueGalleryMessage LoadArt)
                        FeatureEffect.Dispatch GalleryEffect.ClearScreen
                        FeatureEffect.Dispatch (GalleryEffect.QueueGalleryMessage SetRandomArt)
                        FeatureEffect.Dispatch GalleryEffect.OpenGalleryScreen
                    ]
            }
        | DisplayArt art ->
            let dirty =
                match state.ArtSettings.CurrentArt with
                | Some currentArt when currentArt = art -> false
                | _ -> true

            {
                Model =
                    {
                        state with
                            ArtSettings =
                                {
                                    state.ArtSettings with
                                        CurrentArt = Some art
                                }
                            DirtyScreen = dirty
                    }
                Effects = []
            }