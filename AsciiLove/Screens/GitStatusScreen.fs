namespace AsciiLove.Screens
//Op: Auto
open System
open System.IO

open AsciiLove.Domain
open AsciiLove.Messages
open AsciiLove.GitStatus.RepoStatus
open Atomic.Operators
open ConsoleMVU.Contracts
open ConsoleMVU.Runtime

open AsciiLove
open AsciiLove.GitStatus
open MVU
open ConsoleMVU.Contracts.FeatureUpdateResult
//Op: End
module GitStatusScreen =
    type GitStatusDisplayConfig =
        {
            SelectedFileSymbol: string
        }

    type GitStatusFeatureModel =
        {
            GitInfo: GitInfo
            Screen: Screen
            DirtyScreen: bool
        }

    let private toFeatureModel (model: Model) : GitStatusFeatureModel =
        {
            GitInfo = model.GitInfo
            Screen = model.Screen
            DirtyScreen = model.Window.DirtyScreen
        }

    let private fromFeatureModel (featureModel: GitStatusFeatureModel) (model: Model) : Model =
        {
            model with
                GitInfo = featureModel.GitInfo
        }
        |> fun x -> x.SetDirty featureModel.DirtyScreen

    let mutable reTriggerMonitor = fun () -> ()
    let startMonitor dirs =
        let check() =
            dirs
            |> List.choose
                ^ fun x ->
                    let x = RepoStatus.getGitStatusFromCacheStrong x
                    let (np, dirt) = getDirtFromStatus x
                    if np then Some (dirt,x) else None
            |> function
            | [] ->
                [] |> SetGitStatus |> GitStatusMsg |> post
            | hits ->
                hits |> List.map snd |> SetGitStatus |> GitStatusMsg |> post
        check()
        let t = new System.Timers.Timer(2000.0)
        t.Elapsed.Add(fun _ -> check())
        t.Start()
        reTriggerMonitor <-
            fun () ->
                t.Stop()
                check()
                t.Start()
    let view showSelectedFiles (displayConfig: GitStatusDisplayConfig) (m: Model) =
        ConsoleUtils.setSize 12
        Console.SetCursorPosition(0, 0)
        let m = m.GitInfo
        m.GitStatuses
        |> List.iter
            ^ fun (gitStatus: GitRepoStatusReport) ->
                let dirt = getDirtFromStatus gitStatus |> snd
                let repoIsSelected = m.SelectedGitStatus.IsSome && m.SelectedGitStatus.Value = gitStatus
                let meta =
                    m.GitStatusMeta
                    |> Map.tryFind (gitStatus.GetGitStatusMetaKey())
                    <!> GitRepoStatusReportMeta.Default
                printGitRepoStatusReport displayConfig.SelectedFileSymbol m.Side repoIsSelected m.SelectedFile gitStatus meta dirt
                Console_WriteLine("")
        if showSelectedFiles then
            Console_WriteLineFlushed(m.Side.ToString())
            let w (a,b) = Console_WriteLineFlushed($"{a}: {b}")
            w("Path", m.SelectedGitStatus </> _.Path <!> "NoRprt")
            w("RepoPath", m.SelectedFile </> _.RepoPath <!> "NoFile")
            w("FilePath", m.SelectedFile </> _.FilePath <!> "NoFile")
            w("FullPath", m.SelectedFile </> _.FullPath <!> "NoFile")

    let private updateFeatureModel (io: IO.IOActions) (model: GitStatusFeatureModel) (msg: GitStatusMsg) : FeatureUpdateResult<GitStatusFeatureModel, Msg> =
        let gm = model.GitInfo
        let getVisibleFiles () : SelectableFile list =
            gm.GitStatuses
            |?| (fun x -> gm.GetMetaOrDefault (x.GetGitStatusMetaKey()) |> _.Visibility = Visibility.Visible)
            |> List.collect ^ fun x ->
                x.FileBuckets
                |> List.collect _.Files

        match msg with
        | ActOnAllReports action ->
            let setVis newVisibility =
                gm.GitStatusMeta
                |> Map.map
                    ^ fun _ meta ->
                        { meta with Visibility = if meta.Visibility <> Hidden then newVisibility else meta.Visibility }
                |> fun x -> { model with GitInfo = gm.SetGitStatusMeta x }
                |> fun x -> { x with DirtyScreen = true }

            match action with
            | ExpandAll -> setVis Visibility.Visible |> noEffects
            | CollapseAll -> setVis Visibility.Collapsed |> noEffects
        | ActOnSelectedReport action ->
            gm.SelectedGitStatus
            </> fun (selectedStatus: GitRepoStatusReport) ->
                    match action with
                    | DiffFolder ->
                        let psi = new System.Diagnostics.ProcessStartInfo("git", "difftool")
                        psi.WorkingDirectory <- selectedStatus.Path
                        psi.UseShellExecute <- true
                        System.Diagnostics.Process.Start(psi) |> ignore
                        model
                    | GitPush ->
                        let psi = new System.Diagnostics.ProcessStartInfo("git", "push")
                        psi.WorkingDirectory <- selectedStatus.Path
                        psi.UseShellExecute <- true
                        System.Diagnostics.Process.Start(psi) |> ignore
                        model
                    | RememberDir ->
                        selectedStatus.Path |> fun x -> File.WriteAllText(@"c:\temp\lastdir.txt", x)
                        model
                    | ToggleExpand ->
                        let meta = model.GitInfo.GetMetaOrDefault (selectedStatus.GetGitStatusMetaKey())
                        let newMeta =
                            {
                                meta with Visibility = if meta.Visibility = Visibility.Collapsed then Visibility.Visible else Visibility.Collapsed
                            }
                        CLEAR $"Toggling expand for {selectedStatus.Path} to {newMeta.Visibility}"
                        gm.GitStatusMeta
                        |> Map.add (selectedStatus.GetGitStatusMetaKey()) newMeta
                        |> fun x -> { model with GitInfo = gm.SetGitStatusMeta x }
            <!> model
            |> noEffects

        | TrySetSelectedGitRepoStatus x ->
            let effects =
                gm.GitStatuses
                |> List.tryFind ^ fun y -> y.Path.ToLower().TrimEnd('\\') = x.ToLower().TrimEnd('\\')
                |> Option.map (fun x -> FeatureEffect.Dispatch (GitStatusMsg (SetSelectedGitRepoStatus (Some x))))
                |> Option.toList

            {
                Model = model
                Effects = effects
            }
        | SetGitStatus newGitStatuses ->
            let newGitStatuses = newGitStatuses |> List.sortBy _.Path.ToLower()
            let effects = System.Collections.Generic.List<FeatureEffect<Msg>>()

            let baseModel =
                if gm.GitStatuses = newGitStatuses then
                    if model.Screen = GitStatus then
                        match newGitStatuses with
                        | [] -> effects.Add(FeatureEffect.Dispatch (GalleryMsg GalleryMsg.Show))
                        | _ -> ()

                    model
                else
                    let selected =
                        match newGitStatuses with
                        | [] ->
                            effects.Add(FeatureEffect.Dispatch (GalleryMsg GalleryMsg.Show))

                            gm.SelectedGitStatus
                        | x :: _ ->
                            if model.Screen = Gallery then effects.Add(FeatureEffect.Dispatch (goToScreen GitStatus))
                            effects.Add(FeatureEffect.Dispatch (GlobalMsg ClearScreen))
                            gm.SelectedGitStatus
                            |> function
                            | Some x -> Some x
                            | None -> Some x

                    {
                        model with
                            GitInfo =
                                {
                                    gm with
                                        SelectedGitStatus = selected
                                        GitStatuses = newGitStatuses
                                }
                    }

            let curMeta =
                baseModel.GitInfo.GitStatusMeta
            let newMeta =
                baseModel.GitInfo.GitStatuses
                |+| fun x ->
                        x.GetGitStatusMetaKey()
                        |> curMeta.TryFind
                        <!> GitRepoStatusReportMeta.Default
                        |> fun xx -> x.GetGitStatusMetaKey(), xx
                |> Map.ofList

            {
                Model =
                    {
                        baseModel with
                            GitInfo = baseModel.GitInfo.SetGitStatusMeta newMeta
                    }
                Effects = effects |> Seq.toList
            }
        | SetSelectedGitRepoStatus maybeRepo ->
            {
                model with
                    GitInfo = gm.SetSelectedGitStatus maybeRepo
            }
            |> noEffects
        | SetSelectedFile maybeFile ->
            {
                model with
                    GitInfo = gm.SetSelectedFile maybeFile
            }
            |> noEffects
        | VerticalSelectionChange mode ->
            match gm.Side with
            | CursorSide.Left ->
                let newSelectedRepo =
                    match mode with
                    | VerticalSelectionChange.Up ->
                        try
                            gm.SelectedGitStatus
                            |> function
                            | Some selected ->
                                gm.GitStatuses
                                |> List.findIndex ^ fun x -> x = selected
                                |> fun x ->
                                    if x = 0 then gm.GitStatuses.Length - 1
                                    else x - 1
                            | None -> gm.GitStatuses.Length - 1
                            |> swap List.item gm.GitStatuses
                            |> Some
                        with _ -> None
                    | VerticalSelectionChange.Down ->
                        try
                            gm.SelectedGitStatus
                            |> function
                            | Some selected ->
                                gm.GitStatuses
                                |> List.findIndex ^ fun x -> x = selected
                                |> fun x ->
                                    if x = gm.GitStatuses.Length - 1 then 0
                                    else x + 1
                            | None -> gm.GitStatuses.Length - 1
                            |> swap List.item gm.GitStatuses
                            |> Some
                        with _ -> None

                let effects =
                    newSelectedRepo
                    |> Option.map (fun x -> FeatureEffect.Dispatch (GitStatusMsg (SetSelectedGitRepoStatus (Some x))))
                    |> Option.toList

                {
                    Model = model
                    Effects = effects
                }
            | CursorSide.Right ->
                let visibleFiles = getVisibleFiles() |+| _.FilePath
                let nextFile =
                    match mode with
                    | VerticalSelectionChange.Up ->
                        gm.SelectedFile
                        |> function
                        | Some currentFile ->
                            visibleFiles
                            |> List.tryFindIndex ((=) currentFile.FilePath)
                            <!> visibleFiles.Length - 1
                            |> fun x -> x - 1
                            |> swap List.tryItem visibleFiles
                            |> function
                            | Some x -> Some x
                            | None -> visibleFiles.[visibleFiles.Length - 1] |> Some
                        | None ->
                            visibleFiles.[visibleFiles.Length - 1]
                            |> Some
                    | VerticalSelectionChange.Down ->
                        gm.SelectedFile
                        |> function
                        | Some currentFile ->
                            visibleFiles
                            |> List.tryFindIndex ((=) currentFile.FilePath)
                            <!> -1
                            |> fun x -> x + 1
                            |> swap List.tryItem visibleFiles
                            |> function
                            | Some x -> Some x
                            | None -> visibleFiles.[0] |> Some
                        | None ->
                            visibleFiles.[0]
                            |> Some

                let effects =
                    nextFile
                    </> fun x ->
                            {
                                SelectableFile.RepoPath = gm.SelectedGitStatus.Value.Path
                                SelectableFile.FilePath = x
                            }
                    |> Option.map (fun x -> FeatureEffect.Dispatch (GitStatusMsg (SetSelectedFile (Some x))))
                    |> Option.toList

                {
                    Model = model
                    Effects = effects
                }
        | HorizSelectionChange mode ->
            let effects =
                match gm.SelectedFile with
                | Some _ -> []
                | None ->
                    getVisibleFiles()
                    |> List.tryHead
                    </> _.FilePath
                    </> fun x ->
                            {
                                SelectableFile.RepoPath = gm.SelectedGitStatus.Value.Path
                                SelectableFile.FilePath = x
                            }
                    |> Option.map (fun x -> FeatureEffect.Dispatch (GitStatusMsg (SetSelectedFile (Some x))))
                    |> Option.toList

            {
                Model =
                    {
                        model with
                            GitInfo = { gm with Side = mode }
                    }
                Effects = effects
            }
        | IOAction action ->
            match action with
            | Explore ->
                gm.Side
                |> function
                | CursorSide.Left ->
                    gm.SelectedGitStatus
                    <|> fun selectedStatus ->
                            selectedStatus.Path
                            |> DI
                            |> io.ExploreDirectoryInFileExplorer
                | CursorSide.Right ->
                    gm.SelectedFile
                    <|> fun selectedFile ->
                            selectedFile.FullPath
                            |> FI
                            |> io.ExploreInFileExplorer

            noEffects model
        | ActOnSelectedFile action ->
            model.GitInfo.SelectedFile
            <|> fun selectedFile ->
                    let selectedFile = selectedFile.FullPath
                    let git filePath = filePath |> io.RunGitCommand (DI gm.SelectedGitStatus.Value.Path)
                    match action with
                    | StageFile -> git $"add \"%s{selectedFile}\""
                    | DiffFile -> git $"difftool -- %s{selectedFile}"
                    | UnstageFile -> git $"reset HEAD \"%s{selectedFile}\""
                    | OpenInEditor -> io.OpenInEditor(FI selectedFile)

            {
                Model = model
                Effects = [ FeatureEffect.Dispatch (GitStatusMsg RecheckGitStatus) ]
            }
        | RecheckGitStatus ->
            reTriggerMonitor()
            noEffects model

    let private update (io: IO.IOActions) (model: Model) (msg: GitStatusMsg) =
        FeatureAdapter.update
            toFeatureModel
            fromFeatureModel
            (updateFeatureModel io)
            post
            model
            msg

    let hotkeys (gitModel: GitInfo) =
        let isRight = gitModel.Side = CursorSide.Right
        let isLeft = not isRight
        [
            //Op: Auto
            //Op: IndentOnRegex \|
            Hotkey.Create("UpArrow"                     , "Up"              , VerticalSelectionChange.Up   |> GitStatusMsg.VerticalSelectionChange |> Msg.GitStatusMsg)
            Hotkey.Create("DownArrow"                   , "Down"            , VerticalSelectionChange.Down |> GitStatusMsg.VerticalSelectionChange |> Msg.GitStatusMsg)
            Hotkey.Create("E"                           , "Explore"         , IOAction.Explore             |> IOAction                             |> Msg.GitStatusMsg)
            Hotkey.Create2(ConsoleKey.S                 , "Stage"           , StageFile                    |> ActOnSelectedFile                    |> Msg.GitStatusMsg)
            if isRight then Hotkey.Create2(ConsoleKey.D , "Diff"            , DiffFile                     |> ActOnSelectedFile                    |> Msg.GitStatusMsg)
            if isRight then Hotkey.Create2(ConsoleKey.N , "Open in Editor"  , OpenInEditor                 |> ActOnSelectedFile                    |> Msg.GitStatusMsg)
            if isLeft then Hotkey.Create2(ConsoleKey.N  , "Diff Folder"     , DiffFolder                   |> ActOnSelectedReport                  |> Msg.GitStatusMsg)
            Hotkey.Create("U"                           , "Unstage"         , UnstageFile                  |> ActOnSelectedFile                    |> Msg.GitStatusMsg)
            Hotkey.Create2(ConsoleKey.Enter             , "Enter"           , RememberDir                  |> ActOnSelectedReport                  |> Msg.GitStatusMsg)
            Hotkey.Create2(ConsoleKey.RightArrow        , "Navigate Files"  , CursorSide.Right             |> HorizSelectionChange                 |> Msg.GitStatusMsg)
            Hotkey.Create2(ConsoleKey.LeftArrow         , "Navigate Repos"  , CursorSide.Left              |> HorizSelectionChange                 |> Msg.GitStatusMsg)
            Hotkey.Create2(ConsoleKey.Spacebar          , "Expand/Collapse" , ToggleExpand                 |> ActOnSelectedReport                  |> Msg.GitStatusMsg)
            Hotkey.Create2(ConsoleKey.Add               , "Expand All"      , ExpandAll                    |> ActOnAllReports                      |> Msg.GitStatusMsg)
            Hotkey.Create2(ConsoleKey.Subtract          , "Collapse All"    , CollapseAll                  |> ActOnAllReports                      |> Msg.GitStatusMsg)
            Hotkey.Create2(ConsoleKey.UpArrow           , "Push"            , GitPush                      |> ActOnSelectedReport                  |> Msg.GitStatusMsg) |> setMods "Alt"
            //Op: End
        ]


    let getScreenDef (io: IO.IOActions) showSelectedFiles (displayConfig: GitStatusDisplayConfig) : ScreenDef<GitStatusMsg> = {
        View = view showSelectedFiles displayConfig
        Update = update io
        Hotkeys = []
    }