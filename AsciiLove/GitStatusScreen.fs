namespace AsciiLove.Screens
//Op: Auto
open System
open System.IO

open AsciiLove.Domain
open AsciiLove.Messages
open AsciiLove.RepoStatus
open Atomic.Operators

open AsciiLove
//Op: End
module GitStatusScreen =
    open Types
    let startMonitor dirs =
        let check() =
            dirs
            |> List.map RepoStatus.getGitStatusFromCacheStrong
            |> List.map
                ^ fun x ->
                    let (np, dirt) = getDirtFromStatus x
                    if np then Some (dirt,x) else None
            |> List.filter Option.isSome
            |> List.map _.Value
            |> function
            | [] ->
                [] |> SetGitStatus |> GitStatusMsg |> post
            | hits ->
                hits |> List.map snd |> SetGitStatus |> GitStatusMsg |> post
        check()
        let t = new System.Timers.Timer(2000.0)
        t.Elapsed.Add(fun _ -> check())
        t.Start()

    let view (m: Model) =
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
                printGitRepoStatusReport repoIsSelected m.SelectedFile gitStatus meta dirt
                Console_WriteLine("")

        Console_WriteLine(m.Side.ToString())
        Console_WriteLine(m.SelectedGitStatus </> _.Path <!> "NoRprt")
        Console_WriteLine(m.SelectedFile <!> "NoFile")


    let update (model: Model) (msg: GitStatusMsg) =
        let log x = ()
        let gm = model.GitInfo
        match msg with
        | ActOnAllReports action ->
            let setVis newVisibility =
                gm.GitStatusMeta
                |> Map.map
                    ^ fun _ meta ->
                        { meta with Visibility = if meta.Visibility <> Hidden then newVisibility else meta.Visibility }
                |> gm.SetGitStatusMeta
                |> model.SetGitInfo

            //CLEAR "Expanding/Collapsing all repos"
            match action with
            | ExpandAll -> setVis Visibility.Visible
            | CollapseAll -> setVis Visibility.Collapsed
            //|& fun _ -> CLEAR "Expanding/Collapsing all repos"
            |> fun x -> { x with DirtyScreen = true }
        | ActOnSelectedReport action ->
            gm.SelectedGitStatus
            </> fun (selectedStatus: GitRepoStatusReport) ->
                    match action with
                    | RememberDir ->
                        selectedStatus.Path |> fun x -> File.WriteAllText(@"c:\temp\lastdir.txt", x)
                        model
                    | ToggleExpand ->
                        let meta = model.GitInfo.GetMetaOrDefault (selectedStatus.GetGitStatusMetaKey())
                        let newMeta =
                            {
                                meta with Visibility = if meta.Visibility = Visibility.Collapsed then Visibility.Visible else Visibility.Collapsed
                            }
                        CLEAR $"Toggling expand for {selectedStatus.RepoPath} to {newMeta.Visibility}"
                        model.GitInfo.GitStatusMeta
                        |> Map.add (selectedStatus.GetGitStatusMetaKey()) newMeta
                        |> model.GitInfo.SetGitStatusMeta
                        |> model.SetGitInfo
            <!> model
        | TrySetSelectedGitRepoStatus x ->
            model.GitInfo.GitStatuses
            |> List.tryFind ^ fun y -> y.Path.ToLower().TrimEnd('\\') = x.ToLower().TrimEnd('\\')
            <|> fun x -> x |> Some |> SetSelectedGitRepoStatus |> GitStatusMsg |> post

            model
        | SetGitStatus newGitStatuses ->
            let newGitStatuses = newGitStatuses |> List.sortBy _.RepoPath.ToLower()

            if gm.GitStatuses = newGitStatuses then
                log $"a (screen: {model.Screen})"
                if model.Screen = GitStatus then
                    log "b"
                    match newGitStatuses with
                    | [] ->
                        log "c"
                        GalleryMsg.Show
                        |> GalleryMsg
                        |> post
                    | _ ->
                        log "d"
                        ()

                model
            else
                let selected =
                    match newGitStatuses with
                    | [] ->
                        log "e"
                        GalleryMsg.Show
                        |> GalleryMsg
                        |> post

                        model.GitInfo.SelectedGitStatus
                    | x :: xs ->
                        if model.Screen = Gallery then go GitStatus |> post
                        log "f"
                        post (GlobalMsg ClearScreen)
                        model.GitInfo.SelectedGitStatus
                        |> function
                        | Some x -> Some x
                        | None -> Some x

                { model.GitInfo with SelectedGitStatus = selected; GitStatuses = newGitStatuses }
                |> model.SetGitInfo
            |> fun x ->
                let curMeta =
                    x.GitInfo.GitStatusMeta
                let newMeta =
                    x.GitInfo.GitStatuses
                    |+| fun x ->
                            x.GetGitStatusMetaKey()
                            |> curMeta.TryFind
                            <!> GitRepoStatusReportMeta.Default
                            |> fun xx -> x.GetGitStatusMetaKey(), xx
                    |> Map.ofList
                x.GitInfo.SetGitStatusMeta newMeta
                |> x.SetGitInfo
        | SetSelectedGitRepoStatus maybeRepo ->
            maybeRepo
            |> model.GitInfo.SetSelectedGitStatus
            |> model.SetGitInfo

        | VerticalSelectionChange mode ->
            let gm = model.GitInfo
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
                newSelectedRepo |> SetSelectedGitRepoStatus |> GitStatusMsg |> post
                model
            | CursorSide.Right ->
                let allFiles =
                    gm.GitStatuses
                    |?| (fun x -> gm.GetMetaOrDefault (x.GetGitStatusMetaKey()) |> _.Visibility = Visibility.Visible)
                    |> List.collect ^ fun x ->
                        x.FileBuckets
                        |> List.collect _.Files
                match mode with
                | VerticalSelectionChange.Up ->
                    gm.SelectedFile
                    |> function
                    | Some currentFile ->
                        allFiles
                        |> List.tryFindIndex ((=) currentFile)
                        <!> allFiles.Length - 1
                        |> fun x -> x - 1
                        |> swap List.tryItem allFiles
                        |> function
                        | Some x -> Some x
                        | None -> allFiles.[allFiles.Length - 1] |> Some
                        |> gm.SetSelectedFile
                        |> model.SetGitInfo
                    | None ->
                        allFiles.[allFiles.Length - 1]
                        |> Some
                        |> gm.SetSelectedFile
                        |> model.SetGitInfo
                | VerticalSelectionChange.Down ->
                    gm.SelectedFile
                    |> function
                    | Some currentFile ->
                        allFiles
                        |> List.tryFindIndex ((=) currentFile)
                        <!> -1
                        |> fun x -> x + 1
                        |> swap List.tryItem allFiles
                        |> function
                        | Some x -> Some x
                        | None -> allFiles.[0] |> Some
                        |> gm.SetSelectedFile
                        |> model.SetGitInfo
                    | None ->
                        allFiles.[0]
                        |> Some
                        |> gm.SetSelectedFile
                        |> model.SetGitInfo

        | HorizSelectionChange mode ->
            { model.GitInfo with Side = mode }
            |> model.SetGitInfo

    let hotkeys =
        [
            //Op: Auto
            //Op: IndentOnRegex \|
            Hotkey.Create("UpArrow"              , "Up"              , VerticalSelectionChange.Up   |> GitStatusMsg.VerticalSelectionChange |> Msg.GitStatusMsg)
            Hotkey.Create("DownArrow"            , "Down"            , VerticalSelectionChange.Down |> GitStatusMsg.VerticalSelectionChange |> Msg.GitStatusMsg)
            Hotkey.Create2(ConsoleKey.Enter      , "Enter"           , RememberDir                  |> ActOnSelectedReport                  |> Msg.GitStatusMsg)
            Hotkey.Create2(ConsoleKey.RightArrow , "Navigate Files"  , CursorSide.Right             |> HorizSelectionChange                 |> Msg.GitStatusMsg)
            Hotkey.Create2(ConsoleKey.LeftArrow  , "Navigate Repos"  , CursorSide.Left              |> HorizSelectionChange                 |> Msg.GitStatusMsg)
            Hotkey.Create2(ConsoleKey.Spacebar   , "Expand/Collapse" , ToggleExpand                 |> ActOnSelectedReport                  |> Msg.GitStatusMsg)
            Hotkey.Create2(ConsoleKey.Add        , "Expand All"      , ExpandAll                    |> ActOnAllReports                      |> Msg.GitStatusMsg)
            Hotkey.Create2(ConsoleKey.Subtract   , "Collapse All"    , CollapseAll                  |> ActOnAllReports                      |> Msg.GitStatusMsg)
            Hotkey.Create2(ConsoleKey.UpArrow    , "Push"            , Push                         |> ActOnSelectedReport                  |> Msg.GitStatusMsg) |> setMods "Alt"
            //Op: End
        ]