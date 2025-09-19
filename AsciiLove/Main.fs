namespace AsciiLove
//Op: Auto
open System
open System.Threading

open Atomic.Operators

open ArtDomain
open Domain

open Spectre.Console
open Spectre.Console.Json
//Op: End
module Main =
    let init =
        {
            ShowHotkeys = true
            Count = 0
            Busy = false
            Settings =
                {
                    IncrementStep = 1
                    DecrementStep = 1
                    GitDetailLevel = 0
                }
            Screen = Main
            Window =
                {
                    DirtyScreen = false
                    LastHotkeyWrap_Global = None
                    LastHotkeyWrap_Specific = None
                    ShowModel = false
                }
            ArtSettings = ArtDomain.ArtSettings.Empty
            GitInfo = {
                GitStatuses = []
                SelectedGitStatus = None
                GitStatusMeta = Map.empty<GitDomain.GitStatusMetaKey, GitDomain.GitRepoStatusReportMeta>
                Side = CursorSide.Left
                SelectedFile = None
            }
        }

    [<RequireQualifiedAccess>]
    type LoopBehavior =
        | Continue
        | Stop

    module Program =
        let update io raiseMessage (model: Model) (msg: Msg) : Model =
            match msg with
            //Op: Auto
            | GitStatusMsg msg    -> Screens.GitStatusScreen.update io model msg
            | GalleryMsg msg      -> Screens.GalleryScreen.update model msg
            | MainMsg msg         -> Screens.MainScreen.update model msg
            | SettingsMsg msg     -> Screens.SettingsScreen.update model msg
            | HelpMsg msg         -> Screens.HelpScreen.update model msg
            | RaiseMessage (t, m) -> raiseMessage t m; model
            //Op: End
            | GlobalMsg msg ->
                match msg with
                //Op: Auto
                //Op: Sort
                | ClearScreen  -> model.SetDirty true
                | Quit         -> model
                | SetSize size -> ConsoleUtils.setSize size; model
                //Op: End
                | ToggleShowModel ->
                    model
                    |> fun x -> x.SetShowModel (not model.Window.ShowModel)
                    |> fun x -> x.SetDirty true
                | OpenScreen screen ->
                    if screen = model.Screen then
                        model
                    else
                        SetSize 12 |> GlobalMsg |> post

                        { model with Screen = screen }
                        |> fun x -> x.SetDirty true

        let globalHotkeys =
            [
                //Op: Auto
                Hotkey.Create("g" , "Open Gallery"             , GalleryMsg GalleryMsg.Show)
                Hotkey.Create("w" , "Open GS"                  , goToScreen GitStatus)
                Hotkey.Create("m" , "Open Main"                , goToScreen Main)
                Hotkey.Create("s" , "Open Settings"            , goToScreen Settings)
                Hotkey.Create("h" , "Open Help"                , goToScreen Help)
                Hotkey.Create("t" , "[Debug] Toggle Model View", GlobalMsg ToggleShowModel)
                Hotkey.Create("q" , "Quit Application"         , GlobalMsg Quit)
                //Op: End
            ]

        let isKeyAMatch (stringVersion: string) (mods: string) (consoleKey: ConsoleKeyInfo) =
            if consoleKey.KeyChar = char 27 && stringVersion = "Esc" then true
            else
                stringVersion.[0] = consoleKey.KeyChar || stringVersion = consoleKey.Key.ToString()

        let handleKey (model: Model) (key: ConsoleKeyInfo) : unit =
            match model.Screen with
            //Op: Auto
            | Main      -> Screens.MainScreen.hotkeys
            | GitStatus -> Screens.GitStatusScreen.hotkeys
            | Settings  -> Screens.SettingsScreen.hotkeys
            | Help      -> Screens.HelpScreen.hotkeys
            | Gallery   -> Screens.GalleryScreen.hotkeys
            //Op: End
            |> fun hotkeys ->
                hotkeys @ globalHotkeys
                |> List.tryFind
                    ^ fun hotkey -> isKeyAMatch hotkey.Key hotkey.Mods key
            |> Option.iter (fun hotkey -> post (hotkey.Msg))

        let view (model: Model) : Model =
            if model.Window.ShowModel then CLEAR "Reset screen every tick if the model is being shown"
            match model.Screen with
            //Op: Auto
            | Main      -> Screens.MainScreen.view model
            | GitStatus -> Screens.GitStatusScreen.view model
            | Settings  -> Screens.SettingsScreen.view model
            | Help      -> Screens.HelpScreen.view model
            | Gallery   -> Screens.GalleryScreen.view model
            //Op: End

            let model =
                if model.ShowHotkeys && model.Screen <> Gallery then
                    let formatHotkey (hotkey: Hotkey) = $"[{hotkey.Key}] {hotkey.Description}"
                    let printHotkey = formatHotkey >> Console_WriteLine

                    let printHotkeys (hotkeys: Hotkey list) : HotkeyWrapBehavior option =
                        match hotkeys with
                        | [] -> None
                        | hotkeys ->
                            let wide =
                                hotkeys
                                |> List.map formatHotkey
                                |> String.concat " "
                            if wide.Length > Console.WindowWidth then
                                hotkeys |> List.iter printHotkey

                                HotkeyWrapBehavior.NoWrap
                                |> Some
                            else
                                wide
                                |> Console_WriteLine

                                HotkeyWrapBehavior.Wrap
                                |> Some

                    let currentWrapBehavior_Specific =
                        match model.Screen with
                        //Op: Auto
                        | Gallery   -> Screens.GalleryScreen.hotkeys
                        | GitStatus -> Screens.GitStatusScreen.hotkeys
                        | Main      -> Screens.MainScreen.hotkeys
                        | Settings  -> Screens.SettingsScreen.hotkeys
                        | Help      -> Screens.HelpScreen.hotkeys
                        //Op: End
                        |> function
                        | [] -> None
                        | hotkeys ->
                            Console_WriteLine("")
                            Console_WriteLine("")
                            Console_WriteLine($"[{model.Screen} Screen Hotkeys]")
                            hotkeys
                            |> printHotkeys

                    Console_WriteLine("")
                    Console_WriteLine("[Global Hotkeys]")

                    let currentWrapBehavior_Global =
                        globalHotkeys
                        |> printHotkeys

                    let hotkeyWrapBehaviorChanged =
                        model.Window.LastHotkeyWrap_Global <> currentWrapBehavior_Global ||
                        model.Window.LastHotkeyWrap_Specific <> currentWrapBehavior_Specific

                    { model with
                        Window =
                            { model.Window with
                                LastHotkeyWrap_Global = currentWrapBehavior_Global
                                LastHotkeyWrap_Specific = currentWrapBehavior_Specific
                            }
                    }
                    |> fun x -> x.SetDirty (model.Window.DirtyScreen || hotkeyWrapBehaviorChanged)
                else model

            if model.Window.ShowModel then
                Console_WriteLine ""

                model
                |> Model.MiniDump
                |> Newtonsoft.Json.JsonConvert.SerializeObject
                |> fun json ->
                    let json = new JsonText(json)
                    let panel =
                        Panel(json)
                            .Header("Model")
                            .Collapse()
                            .RoundedBorder()
                            .BorderColor(Color.White)
                    AnsiConsole.Write(panel)
            model

        let step io raiseMessage (keyOpt: ConsoleKeyInfo option) (prevModel: Model): Model * LoopBehavior =
            keyOpt
            <|> fun key -> handleKey prevModel key

            let mutable newModel = prevModel
            let mutable loopBehavior = LoopBehavior.Continue
            let mutable msgOpt : Msg = Unchecked.defaultof<Msg>

            while messageQueue.TryDequeue &msgOpt do
                let updated = update io raiseMessage newModel msgOpt
                newModel <- updated
                match msgOpt with
                | GlobalMsg Quit -> loopBehavior <- LoopBehavior.Stop
                | _ -> ()

            newModel, loopBehavior

        let loop io serialize (modelUpdateMessageCallbacks: ModelUpdateMessageCallback list) raiseMessage forceView (prevModel: Model) =
            let rec go forceView (prevModel: Model) =
                let keyOpt = ConsoleUtils.maybeGetKey()

                let newModel, continueLoop = step io raiseMessage keyOpt prevModel

                let wasDirty = newModel.Window.DirtyScreen

                let newModel =
                    if newModel.Window.DirtyScreen then
                        CLEAR "Screen is dirty"
                        newModel.SetDirty false
                    else newModel

                if newModel <> prevModel then
                    modelUpdateMessageCallbacks
                    |% fun mum ->
                        mum.Callback newModel
                        |> fun x ->
                            serialize x
                            |> raiseMessage mum.Name

                let newModel =
                    if newModel <> prevModel || forceView || wasDirty then
                        view newModel
                    else newModel

                match continueLoop with
                | LoopBehavior.Stop -> CLEAR "Program loop stopped"
                | LoopBehavior.Continue ->
                    Thread.Sleep(1)
                    go false newModel
            go forceView prevModel

    let configLogger() =
        let cfg = Config.getConfig()
        let log =
            let lc = cfg.LoggingConfig
            getLogger lc
        SetLogger log

    [<EntryPoint>]
    let main args =
        configLogger()

        Serilog.Debugging.SelfLog.Enable(fun msg -> Console.WriteLine(msg); logDebug msg)

        ConsoleUtils.setSize 12

        let mqtt = FMQTT.LocalBroker.ConnectToLocal()

        mqtt.SubscribeToTopicBasic "SelectedGitStatusPath" ^ fun x -> TrySetSelectedGitRepoStatus x |> GitStatusMsg |> post

        [
            GalleryMsg (SetDisplayMode ArtDisplayMode.Text)
            GalleryMsg (SetFilter ArtFilter.All)
            GlobalMsg ClearScreen
            goToScreen GitStatus
        ]
        |> List.iter post

        let recurringMessages =
            [
                60000, GalleryMsg SetRandomArt
                3000, GalleryMsg CheckForChanges
            ]

        recurringMessages
        |% fun (interval, msg) ->
            let timer = new System.Timers.Timer(interval)
            timer.Elapsed.Add(fun _ -> post msg)
            timer.Start()

        let modelUpdateMessages =
            let go name fn = { Name = name; Callback = fn }
            [
                go "SelectedGitStatusPath" ^ fun x -> x.GitInfo.SelectedGitStatus </> _.Path <!> ""
            ]

        let assertIsGitRepoDir(path: string) : unit =
            if System.IO.Directory.Exists(path) then
                if GitDomain.isGitRepository path then
                    ()
                else
                    failwith $"The directory '{path}' is not a git repository."
            else
                raise (System.IO.DirectoryNotFoundException($"The directory '{path}' does not exist."))

        args
        |? fun x -> x.Length > 0 //remove empty args
        |+| fun x ->
            assertIsGitRepoDir x
            x
        |> Seq.toList
        |> Screens.GitStatusScreen.startMonitor

        let serialize (o: obj) = Newtonsoft.Json.JsonConvert.SerializeObject(o, Newtonsoft.Json.Formatting.Indented)

        let io =
            {
                IO.IOActions.ExploreInFileExplorer = IO.ThingsYouCanDoToAFile.ExploreInFileExplorer
                //IO.IOActions.OpenInDefaultApp = IO.ThingsYouCanDoToAFile.Open
                IO.IOActions.ExploreDirectoryInFileExplorer = IO.ThingsYouCanDoToADirectory.ExploreInFileExplorer
                IO.IOActions.RunGitCommand = IO.RunGitCommand
            }
        Program.loop io serialize modelUpdateMessages mqtt.PublishMessage true init
        0