namespace AsciiLove
//Op: Auto
open System
open System.Threading

open Atomic.Operators

open ArtDomain
open Domain

open MQTTnet.Client
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
                }
            ShowModel = false
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
        let update raiseMessage (model: Model) (msg: Msg) : Model =
            match msg with
            //Op: Auto
            | GitStatusMsg msg -> Screens.GitStatusScreen.update model msg
            | GalleryMsg msg   -> Screens.GalleryScreen.update model msg
            | MainMsg msg      -> Screens.MainScreen.update model msg
            | SettingsMsg msg  -> Screens.SettingsScreen.update model msg
            | HelpMsg msg      -> Screens.HelpScreen.update model msg
            //Op: End
            | RaiseMessage (t, m) ->
                raiseMessage t m
                model
            | GlobalMsg msg ->
                match msg with
                //Op: Auto
                //Op: Sort
                | ClearScreen     -> model.SetDirty true
                | Quit            -> model
                | SetSize size    -> ConsoleUtils.setSize size; model
                | ToggleShowModel -> { model with ShowModel = not model.ShowModel } |> fun x -> x.SetDirty true
                //Op: End
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
                Hotkey.Create("g"    , "Open Gallery"             , GalleryMsg GalleryMsg.Show)
                Hotkey.Create("w"    , "Open GS"                  , go GitStatus)
                Hotkey.Create("m"    , "Open Main"                , go Main)
                Hotkey.Create("s"    , "Open Settings"            , go Settings)
                Hotkey.Create("h"    , "Open Help"                , go Help)
                Hotkey.Create("t"    , "[Debug] Toggle Model View", GlobalMsg ToggleShowModel)
                Hotkey.Create("q"    , "Quit Application"         , GlobalMsg Quit)
                //Hotkey.Create("Esc", "Open Main"                , go Main)
                //Op: End
            ]

        let isKeyAMatch (stringVersion: string) (mods: string) (consoleKey: ConsoleKeyInfo) =
            if consoleKey.KeyChar = char 27 && stringVersion = "Esc" then true
            else
                stringVersion.[0] = consoleKey.KeyChar || stringVersion = consoleKey.Key.ToString()

        let handleKey (model: Model) (key: ConsoleKeyInfo) : unit =
            match model.Screen with
            | Main -> Screens.MainScreen.hotkeys
            | GitStatus -> Screens.GitStatusScreen.hotkeys
            | Settings -> Screens.SettingsScreen.hotkeys
            | Help -> Screens.HelpScreen.hotkeys
            | Gallery -> Screens.GalleryScreen.hotkeys
            |> fun hotkeys ->
                hotkeys @ globalHotkeys
                |> List.tryFind
                    ^ fun hotkey ->
                        let fixedHotkeyAsChar =
                            match hotkey.Key with
                            | "Esc" -> char 27
                            | x -> x.[0]
                        //fixedHotkeyAsChar = key.KeyChar
                        isKeyAMatch hotkey.Key hotkey.Mods key
            |> Option.iter (fun hotkey -> post (hotkey.Msg))

        let view (model: Model) : Model =
            if model.ShowModel then CLEAR "Reset screen every tick if the model is being shown"
            match model.Screen with
            | Main -> Screens.MainScreen.view model
            | GitStatus -> Screens.GitStatusScreen.view model
            | Settings -> Screens.SettingsScreen.view model
            | Help -> Screens.HelpScreen.view model
            | Gallery -> Screens.GalleryScreen.view model

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

                    //Console_WriteLine("")
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

            if model.ShowModel then
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

        let step raiseMessage (keyOpt: ConsoleKeyInfo option) (prevModel: Model): Model * LoopBehavior =
            keyOpt
            |> Option.iter
                ^ fun key ->
                    handleKey prevModel key
            let mutable newModel = prevModel
            let mutable loopBehavior = LoopBehavior.Continue
            let mutable msgOpt : Msg = Unchecked.defaultof<Msg>

            while messageQueue.TryDequeue &msgOpt do
                let updated = update raiseMessage newModel msgOpt
                newModel <- updated
                match msgOpt with
                | GlobalMsg Quit -> loopBehavior <- LoopBehavior.Stop
                | _ -> ()

            newModel, loopBehavior

        let loop srlz (mums: MUM list) raiseMessage forceView (prevModel: Model) =
            let rec go forceView (prevModel: Model) =
                let keyOpt = getKeyOpt()

                let newModel, continueLoop = step raiseMessage keyOpt prevModel

                let wasDirty = newModel.Window.DirtyScreen

                let newModel =
                    if newModel.Window.DirtyScreen then
                        CLEAR "Screen is dirty"
                        newModel.SetDirty false
                    else newModel

                if newModel <> prevModel then
                    mums
                    |% fun mum ->
                        mum.Fn newModel
                        |> fun x -> srlz x |> raiseMessage mum.Name

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

    let (|AsPayloadString|) (x: MqttApplicationMessageReceivedEventArgs) =
        x.ApplicationMessage.PayloadSegment
        |> Seq.toArray
        //x.ApplicationMessage.Payload
        |> System.Text.Encoding.UTF8.GetString

    [<EntryPoint>]
    let main args =
        Serilog.Debugging.SelfLog.Enable(fun msg -> Console.WriteLine(msg))
        let configLogger() =
            let cfg = Config.getConfig()
            let log =
                let lc = cfg.LoggingConfig
                getLogger lc
            SetLogger log
        configLogger()
        Logging.logDebug "debug"
        //Logging.logInfo "info"
        //Logging.logWarn "warn"
        //Logging.logError (Exception("test exception")) "error"
        //Logging.logVerbose "verbose"

        ConsoleUtils.setSize 12
        let m = FMQTT.LocalBroker.ConnectToLocal()

        m.SubscribeToTopicBasic "SelectedGitStatusPath" ^ fun x -> TrySetSelectedGitRepoStatus x |> GitStatusMsg |> post

        [
            GalleryMsg (SetDisplayMode ArtDisplayMode.Text)
            GalleryMsg (SetFilter ArtFilter.All)
            GlobalMsg ClearScreen
            go GitStatus
        ]
        |> List.iter post

        let recurringMessages =
            [
                60000, GalleryMsg SetRandomArt
                3000, GalleryMsg CheckForChanges
            ]

        recurringMessages
        |> List.iter (fun (interval, msg) ->
            let timer = new System.Timers.Timer(interval)
            timer.Elapsed.Add(fun _ -> post msg)
            timer.Start()
        )

        let modelUpdateMessages =
            let go name fn = { Name = name; Fn = fn }
            [
                go "SelectedGitStatusPath" ^ fun x -> x.GitInfo.SelectedGitStatus </> _.Path <!> ""
            ]

        args
        |> Seq.where ^ fun x -> x.Length > 0
        |> Seq.toList
        |> Screens.GitStatusScreen.startMonitor
        let srlz (o: obj) = Newtonsoft.Json.JsonConvert.SerializeObject(o, Newtonsoft.Json.Formatting.Indented)
        Program.loop srlz modelUpdateMessages m.PublishMessage true init
        0