namespace ConsoleMVU.Gallery

open AsciiLove

module GalleryFeature =
    let view = Console.GalleryConsole.view
    let update = Core.GalleryCore.update
    let hotkeys = Console.GalleryConsole.hotkeys