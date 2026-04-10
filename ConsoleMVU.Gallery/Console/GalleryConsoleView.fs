namespace ConsoleMVU.Gallery.Console

open System
open ConsoleMVU.Gallery.GalleryDomain
open Spectre.Console

module GalleryConsoleView =
    module ColorPrint =
        let printWithColor (color: string) (line: string) =
            let reset = "\u001b[0m"
            Console.WriteLine($"{color}%s{line}{reset}")

    let renderImage (file: string) =
        let image = new CanvasImage(file)
        AnsiConsole.Write(image)

    let view (clear: string -> unit) (setSize: int -> unit) (artSettings: ArtSettings) =
        Console.SetCursorPosition(0, 0)
        match artSettings.CurrentArt with
        | None -> ()
        | Some art ->
            let printText() =
                clear "Before printing art in GalleryConsoleView.view"
                let renderInfo = art.RenderInfo()
                setSize renderInfo.Size
                ColorPrint.printWithColor renderInfo.ColorCode renderInfo.Text

            match artSettings.ArtDisplayMode, art.Image with
            | ArtDisplayMode.Text, _ -> printText()
            | ArtDisplayMode.ImageIfAvailable, Some image -> renderImage image
            | ArtDisplayMode.ImageIfAvailable, None -> printText()