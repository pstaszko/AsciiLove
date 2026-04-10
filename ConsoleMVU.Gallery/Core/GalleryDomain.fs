namespace ConsoleMVU.Gallery

module GalleryDomain =
    open System
    open System.IO

    type SupportedConsoleColors =
        | Black
        | DarkBlue
        | DarkGreen
        | DarkCyan
        | DarkRed
        | DarkMagenta
        | DarkYellow
        | Gray
        | DarkGray
        | Blue
        | Green
        | Cyan
        | Red
        | Magenta
        | Yellow
        | White
        | Pink

    module UnionStuff =
        open Microsoft.FSharp.Reflection

        let toString (x: 'a) =
            let (case, _) = FSharpValue.GetUnionFields(x, typeof<'a>)
            case.Name

        let fromString<'a> (s: string) =
            let s = s.ToLower()
            match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name.ToLower() = s) with
            | [| case |] -> Some(FSharpValue.MakeUnion(case, [||]) :?> 'a)
            | _ -> None

    type RenderInfo =
        {
            ColorCode: string
            Text: string
            Size: int
        }

    type Art =
        {
            TxtFullPath: string
            TxtLastWrite: DateTime
            Color: SupportedConsoleColors
            Image: string option
            ImageLastWrite: DateTime option
        }
        member this.RenderInfo() =
            let isInt (txt: string) =
                match Int32.TryParse(txt, null) with
                | true, _ -> true
                | _ -> false

            let colorCode =
                this.Color
                |> function
                | SupportedConsoleColors.Black -> "\u001b[30m"
                | SupportedConsoleColors.DarkBlue -> "\u001b[34m"
                | SupportedConsoleColors.DarkGreen -> "\u001b[32m"
                | SupportedConsoleColors.DarkCyan -> "\u001b[36m"
                | SupportedConsoleColors.DarkRed -> "\u001b[31m"
                | SupportedConsoleColors.DarkMagenta -> "\u001b[35m"
                | SupportedConsoleColors.DarkYellow -> "\u001b[33m"
                | SupportedConsoleColors.Gray -> "\u001b[37m"
                | SupportedConsoleColors.DarkGray -> "\u001b[90m"
                | SupportedConsoleColors.Blue -> "\u001b[94m"
                | SupportedConsoleColors.Green -> "\u001b[92m"
                | SupportedConsoleColors.Cyan -> "\u001b[96m"
                | SupportedConsoleColors.Red -> "\u001b[91m"
                | SupportedConsoleColors.Magenta -> "\u001b[95m"
                | SupportedConsoleColors.Yellow -> "\u001b[93m"
                | SupportedConsoleColors.White -> "\u001b[97m"
                | SupportedConsoleColors.Pink -> "\u001b[95m"

            File.ReadAllLines(this.TxtFullPath)
            |> Seq.toList
            |> function
            | [] -> failwith $"Art file {this.TxtFullPath} is empty"
            | x :: lines when isInt x -> lines, Int32.Parse(x)
            | lines -> lines, 10
            |> fun (lines, size) ->
                let lines = String.Join(Environment.NewLine, lines)
                {
                    ColorCode = colorCode
                    Text = lines
                    Size = size
                }

    let pathToArt color (filePath: string) =
        let colorFolderPath = (FileInfo filePath).Directory.FullName
        let fileName = Path.GetFileNameWithoutExtension(filePath)
        let imagePath =
            [
                "jpg"
                "png"
                "gif"
                "jpeg"
                "bmp"
            ]
            |> Seq.map (fun ext -> $"%s{fileName}.%s{ext}")
            |> Seq.collect (fun localFileName -> Directory.GetFiles(colorFolderPath, localFileName))
            |> Seq.tryHead

        let imageLastWrite =
            match imagePath with
            | Some path -> Some(File.GetLastWriteTime(path))
            | None -> None

        {
            TxtFullPath = filePath
            TxtLastWrite = File.GetLastWriteTime(filePath)
            Color = color
            Image = imagePath
            ImageLastWrite = imageLastWrite
        }

    let getAllArt () : Art list =
        Directory.GetDirectories(@"C:\Dev\PowerShell\Art")
        |> Seq.collect
            (fun colorFolderPath ->
                let di = DirectoryInfo colorFolderPath
                let color =
                    UnionStuff.fromString<SupportedConsoleColors> di.Name
                    |> function
                    | Some c -> c
                    | None -> failwith $"{di.Name} is not a valid ConsoleColor"

                Directory.GetFiles(colorFolderPath, "*.txt")
                |> Seq.map (fun filePath -> pathToArt color filePath))
        |> Seq.toList

    [<RequireQualifiedAccess>]
    type ArtFilter =
        | All
        | ImageOnly
        | Newest

    type ArtDisplayMode =
        | ImageIfAvailable
        | Text

    type ArtSettings =
        {
            AllArt: Art list
            CurrentArt: Art option
            ArtDisplayMode: ArtDisplayMode
            ArtFilter: ArtFilter
        }
        static member Empty =
            {
                AllArt = []
                CurrentArt = None
                ArtDisplayMode = ArtDisplayMode.ImageIfAvailable
                ArtFilter = ArtFilter.ImageOnly
            }

        member this.AcceptableArt() =
            match this.ArtFilter with
            | ArtFilter.Newest ->
                this.AllArt
                |> List.sortByDescending (fun art -> File.GetLastWriteTime(art.TxtFullPath))
                |> function
                | [] -> []
                | x :: _ -> [ x ]
            | ArtFilter.All -> this.AllArt
            | ArtFilter.ImageOnly -> this.AllArt |> List.filter (fun art -> art.Image.IsSome)

        member this.ArtOffsetBy count : Art option =
            let acceptableArt = this.AcceptableArt()

            match acceptableArt, this.CurrentArt with
            | [], _ -> None
            | acceptableArt, Some currentArt ->
                let currentIndex = acceptableArt |> List.findIndex (fun a -> a = currentArt)
                let nextIndex =
                    let x = currentIndex + count
                    if x < 0 then
                        acceptableArt.Length - 1
                    else
                        x % List.length acceptableArt

                acceptableArt.[nextIndex] |> Some
            | acceptableArt, None -> acceptableArt.[0] |> Some

        member this.NextArt() : Art option = this.ArtOffsetBy 1

        member this.PreviousArt() : Art option = this.ArtOffsetBy -1

        member this.SetRandomArt() =
            let acceptableArt =
                let artToConsider = this.AcceptableArt()

                match this.CurrentArt with
                | None -> artToConsider
                | Some x -> artToConsider |> List.filter (fun art -> art <> x)

            let randomArt =
                if List.isEmpty acceptableArt then
                    None
                else
                    acceptableArt |> List.randomChoice |> Some

            { this with CurrentArt = randomArt }
