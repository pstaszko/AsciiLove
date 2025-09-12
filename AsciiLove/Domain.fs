namespace AsciiLove
open Atomic.Operators

module ArtDomain =
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

        let toString (x:'a) =
            let (case, _ ) = FSharpValue.GetUnionFields(x, typeof<'a>)
            case.Name

        let fromString<'a> (s:string) =
            let s = s.ToLower()
            match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name.ToLower() = s) with
            |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
            |_ -> None

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
        } with
            member this.RenderInfo() =
                let isInt (txt: string) =
                    match System.Int32.TryParse(txt, null) with
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
                | x :: lines when isInt x -> lines, System.Int32.Parse(x)
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
        let fileName = System.IO.Path.GetFileNameWithoutExtension(filePath)
        let imagePath =
                [
                    "jpg"
                    "png"
                    "gif"
                    "jpeg"
                    "bmp"
                ]
                |> Seq.map (fun ext -> $"%s{fileName}.%s{ext}")
                |> Seq.collect ^ fun fileName -> Directory.GetFiles(colorFolderPath, fileName)
                |> Seq.tryHead
        let imageLastWrite =
            match imagePath with
            | Some path -> Some (System.IO.File.GetLastWriteTime(path))
            | None -> None
        {
            TxtFullPath = filePath
            TxtLastWrite = System.IO.File.GetLastWriteTime(filePath)
            Color = color
            Image = imagePath
            ImageLastWrite = imageLastWrite
        }

    let getAllArt () : Art list =
        System.IO.Directory.GetDirectories(@"C:\Dev\PowerShell\Art")
        |> Seq.collect
            ^ fun colorFolderPath ->
                let di = System.IO.DirectoryInfo colorFolderPath
                let color =
                    UnionStuff.fromString<SupportedConsoleColors> di.Name
                    |> function
                    | Some c -> c
                    | None -> failwith $"{di.Name} is not a valid ConsoleColor"
                System.IO.Directory.GetFiles(colorFolderPath, "*.txt")
                |> Seq.map
                    ^ fun filePath ->
                        pathToArt color filePath
        |> Seq.toList
        |> fun x -> x

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
                | x :: _ -> [x]
            | ArtFilter.All ->
                this.AllArt
            | ArtFilter.ImageOnly ->
                this.AllArt |> List.filter (fun art -> art.Image.IsSome)

        member this.ArtOffsetBy count : Art option =
            let acceptableArt = this.AcceptableArt()

            match acceptableArt, this.CurrentArt with
            | [], _ -> None
            | acceptableArt, Some currentArt ->
                let currentIndex = acceptableArt |> List.findIndex (fun a -> a = currentArt)
                let nextIndex =
                    let x = (currentIndex + count)
                    if x < 0 then
                        acceptableArt.Length - 1
                    else
                        x % List.length acceptableArt
                acceptableArt.[nextIndex]
                |> Some
            | acceptableArt, None ->
                acceptableArt.[0]
                |> Some

        member this.NextArt() : Art option = this.ArtOffsetBy 1

        member this.PreviousArt() : Art option = this.ArtOffsetBy -1

        member this.SetRandomArt() =
            let acceptableArt =
                let artToConsider = this.AcceptableArt()

                match this.CurrentArt with
                | None -> artToConsider
                | Some x -> artToConsider |> List.filter (fun art -> art <> x)

            let randomArt =
                if List.isEmpty acceptableArt then None
                else acceptableArt |> List.randomChoice |> Some

            { this with CurrentArt = randomArt }

[<AutoOpen>]
module Domain =
    open ArtDomain

    type Screen =
        | Main
        | Settings
        | GitStatus
        | Help
        | Gallery

    type SavedSettings =
        {
            IncrementStep: int
            DecrementStep: int
            GitDetailLevel: int
        }

    [<RequireQualifiedAccess>]
    type CursorSide =
        | Right
        | Left

    type GitInfo =
        {
            GitStatuses: GitRepoStatusReport list
            SelectedGitStatus: GitRepoStatusReport option
            GitStatusMeta: Map<GitStatusMetaKey, GitRepoStatusReportMeta>
            Side: CursorSide
            SelectedFile: string option
        }
        member this.SetGitStatusMeta x = { this with GitStatusMeta = x }
        member this.SetSelectedGitStatus x = { this with SelectedGitStatus = x }
        member this.SetSelectedFile x = { this with SelectedFile = x }
        member this.GetMetaOrDefault (repoPath: GitStatusMetaKey) =
            this.GitStatusMeta
            |> Map.tryFind repoPath
            <!> GitRepoStatusReportMeta.Default
            
    type HotkeyWrapBehavior =
        | Wrap
        | NoWrap

        //member this.ReplaceAReport (newReport: GitRepoStatusReport) =
        //    let n =
        //        this.GitStatuses
        //        |+| fun existingReport ->
        //            if existingReport.RepoPath = newReport.RepoPath then
        //                newReport
        //            else
        //                existingReport
        //    { this with GitStatuses = n }
    
    type ScreenHouseKeeping =
        {
            DirtyScreen: bool
            LastHotkeyWrap_Specific: HotkeyWrapBehavior option
            LastHotkeyWrap_Global: HotkeyWrapBehavior option
            ShowModel: bool
        }

    type Model =
        {
            ShowHotkeys: bool
            Count: int
            Busy: bool
            Settings: SavedSettings
            ArtSettings: ArtSettings
            Screen: Screen
            Window: ScreenHouseKeeping
            GitInfo: GitInfo
        }
        
        member this.SetDirty newValue = { this with Window = { this.Window with DirtyScreen = newValue } }
        member this.SetShowModel newValue = { this with Window = { this.Window with ShowModel = newValue } }
        member this.SetGitInfo x = { this with GitInfo = x }
        static member MiniDump (model: Model) =
            {
                model with
                    GitInfo = { model.GitInfo with GitStatuses = [] }
                    ArtSettings =
                        {
                            model.ArtSettings with
                                AllArt = []
                        }
            }

    type MUM =
        {
            Name: string
            Fn: Model -> obj
        }

[<AutoOpen>]
module Messages =
    open Domain
    open ArtDomain
    type GalleryMsg =
        | SetDisplayMode of ArtDisplayMode
        | SetFilter of ArtFilter
        | Next
        | Previous
        | Show
        | LoadArt
        | SetRandomArt
        | CheckForChanges
        | ClearArt
        | OpenImageText
        | ExploreImageText
        | OpenImageFileIfExists
        | DisplayArt of Art

    type GlobalMsg =
        | OpenScreen of Screen
        | SetSize of int
        | ClearScreen
        | Quit
        | ToggleShowModel

    type MainMsg =
        | Increment
        | Decrement
        | DelayedIncrement
        | DelayedIncrementCompleted

    [<RequireQualifiedAccess>]
    type VerticalSelectionChange =
        | Up
        | Down

    type GitRepoStatusReportAction =
        | RememberDir
        | ToggleExpand
        | GitPush

    type AllReportsAction =
        | ExpandAll
        | CollapseAll

    type GitStatusMsg =
        | SetGitStatus of GitRepoStatusReport list
        | VerticalSelectionChange of VerticalSelectionChange
        | HorizSelectionChange of CursorSide
        | SetSelectedGitRepoStatus of GitRepoStatusReport option
        | TrySetSelectedGitRepoStatus of string
        | ActOnSelectedReport of GitRepoStatusReportAction
        | ActOnAllReports of AllReportsAction
        | SetSelectedFile of string option

    type SettingsMsg =
        | IncIncrementStep
        | DecIncrementStep
        | IncDecrementStep
        | DecDecrementStep

    type HelpMsg =
        | CloseHelp


    type Topic = string

    type Message = string

    type Msg =
        | MainMsg of MainMsg
        | SettingsMsg of SettingsMsg
        | HelpMsg of HelpMsg
        | GlobalMsg of GlobalMsg
        | GalleryMsg of GalleryMsg
        | GitStatusMsg of GitStatusMsg
        | RaiseMessage of Topic * Message

    type Hotkey =
        {
            Key: string
            Description: string
            Msg: Msg
            Mods: string
        }
        static member Create(key: string, description: string, msg: Msg) =
            { Key = key; Description = description; Msg = msg; Mods = "" }

        static member Create2(key: System.ConsoleKey, description: string, msg: Msg) =
            { Key = key |> string; Description = description; Msg = msg; Mods = "" }

    let setMods (mods: string) (hotkey: Hotkey) =
        { hotkey with Mods = mods }

    let go (screen: Screen) = GlobalMsg (OpenScreen screen)

[<AutoOpen>]
module MessageQueue =
    open Messages
    open System.Collections.Concurrent

    let messageQueue = ConcurrentQueue<Msg>()

    let post (msg: Msg) : unit =
        //Logging.logDebug msg
        messageQueue.Enqueue msg