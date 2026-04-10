namespace AsciiLove
open Atomic.Operators
open StarlyCode.CasePipes
open AsciiLove.GitStatus

[<AutoOpen>]
module Domain =
    open ConsoleMVU.Gallery.GalleryDomain

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

    type HotkeyWrapBehavior =
        | Wrap
        | NoWrap

    type HotkeyWork =
        {
            LinesToPrint: string list
            WrapBehavior: HotkeyWrapBehavior
        }

    type ScreenHouseKeeping =
        {
            DirtyScreen: bool
            LastHotkeyWrap_Specific: HotkeyWrapBehavior option
            LastHotkeyWrap_Global: HotkeyWrapBehavior option
            HotkeyText_Specific: string list
            HotkeyText_Global: string list
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

    type ModelUpdateMessageCallback =
        {
            Name: string
            Callback: Model -> obj
        }

[<AutoOpen>]
module Messages =
    open Domain
    open ConsoleMVU.Gallery.GalleryDomain

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
        | DiffFolder
        | GitPush

    type AllReportsAction =
        | ExpandAll
        | CollapseAll

    type FileAction =
        | OpenInEditor
        | StageFile
        | DiffFile
        | UnstageFile

    type IOAction =
        | Explore

    type GitStatusMsg =
        //Op: Auto
        //Op: Sort
        | ActOnAllReports of AllReportsAction
        | ActOnSelectedFile of FileAction
        | ActOnSelectedReport of GitRepoStatusReportAction
        | HorizSelectionChange of CursorSide
        | IOAction of IOAction
        | RecheckGitStatus
        | SetGitStatus of GitRepoStatusReport list
        | SetSelectedFile of SelectableFile option
        | SetSelectedGitRepoStatus of GitRepoStatusReport option
        | TrySetSelectedGitRepoStatus of string
        | VerticalSelectionChange of VerticalSelectionChange
        //Op: End

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
        | GalleryMsg of ConsoleMVU.Gallery.GalleryMessages.GalleryMsg
        | GitStatusMsg of GitStatusMsg
        | RaiseMessage of Topic * Message
    let private fixKeyName = CasePipe.SpacifyName >> CasePipe.TitleCase
    type Hotkey =
        {
            Key: string
            DisplayName: string
            Description: string
            Msg: Msg
            Mods: string
        }
        static member Create(key: string, description: string, msg: Msg) =
            { Key = key; DisplayName = key |> fixKeyName ; Description = description; Msg = msg; Mods = "" }

        static member Create2(key: System.ConsoleKey, description: string, msg: Msg) =
            { Key = key.ToString(); DisplayName = key.ToString() |> fixKeyName; Description = description; Msg = msg; Mods = "" }

    let setMods (mods: string) (hotkey: Hotkey) =
        { hotkey with Mods = mods }

    let goToScreen (screen: Screen) = GlobalMsg (OpenScreen screen)

[<AutoOpen>]
module MessageQueue =
    open Messages
    open System.Collections.Concurrent

    let messageQueue = ConcurrentQueue<Msg>()

    let post (msg: Msg) : unit =
        //Logging.logDebug msg
        messageQueue.Enqueue msg