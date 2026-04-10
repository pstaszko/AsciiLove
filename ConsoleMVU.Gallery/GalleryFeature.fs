namespace ConsoleMVU.Gallery

open ConsoleMVU.Gallery.GalleryDomain
open ConsoleMVU.Gallery.GalleryMessages

module GalleryFeature =
    type State = ArtSettings
    type Msg = GalleryMsg

    let update = ConsoleMVU.Gallery.Core.GalleryFeature.update
    let hotkeys = ConsoleMVU.Gallery.Console.GalleryHotkeys.all
    let view = ConsoleMVU.Gallery.Console.GalleryConsoleView.view