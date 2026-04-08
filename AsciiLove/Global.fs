namespace AsciiLove
open System
module IO =
    open System.IO
    let noop x = ()
    type RepoDirectory = DirectoryInfo
    type IOActions =
        {
            ExploreInFileExplorer: FileInfo -> unit
            OpenInEditor: FileInfo -> unit
            ExploreDirectoryInFileExplorer: DirectoryInfo -> unit
            RunGitCommand: RepoDirectory -> string -> unit
            SerializeIndented: obj -> string
        }

    let RunGitCommand (workingDirectory: RepoDirectory) (cmd: string) =
        let psi = new System.Diagnostics.ProcessStartInfo("git", cmd)
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.UseShellExecute <- false
        psi.WorkingDirectory <- workingDirectory.FullName
        let processx = System.Diagnostics.Process.Start(psi)
        let output = processx.StandardOutput.ReadToEnd()
        let error = processx.StandardError.ReadToEnd()
        processx.WaitForExit() |> ignore
        if processx.ExitCode = 0 then
            Logging.logInfo $"Git command succeeded: git {cmd}\n{output}"
        else
            Logging.logWarn $"Git command failed (exit code {processx.ExitCode}): git {cmd}\n{error}"

    type ThingsYouCanDoToADirectory =
        static member ExploreInFileExplorer (path: DirectoryInfo) =
            if path.Exists then
                let args = $"\"{path.FullName}\""
                System.Diagnostics.Process.Start("explorer.exe", args) |> ignore
            else
                Logging.logWarn $"Directory does not exist: {path.FullName}"

    type ThingsYouCanDoToAFile =
        static member OpenInEditor editorPath (path: FileInfo) =
            if path.Exists then
                if String.IsNullOrWhiteSpace(editorPath) then
                    Logging.logWarn "No editor set in EDITOR environment variable."
                else
                    let args = $"\"{path.FullName}\""
                    System.Diagnostics.Process.Start(editorPath, args) |> ignore
            else
                Logging.logWarn $"File does not exist: {path.FullName}"

        static member ExploreInFileExplorer (path: FileInfo) =
            if path.Exists then
                let args = $"/e,/select,\"{path.FullName}\""
                System.Diagnostics.Process.Start("explorer.exe", args) |> ignore
            else
                Logging.logWarn $"File does not exist: {path.FullName}"

[<AutoOpen>]
module Global =
    type FI = System.IO.FileInfo
    type DI = System.IO.DirectoryInfo

    let swap fn arg1 arg2 = fn arg2 arg1

    let CLEAR (reason: string) =
        Logging.logVerbose $"RealClear: {reason}"
        Console.Clear()
        Console.Write("\x1b[3J")
        Console.Clear()

    let BREAKK x = x

    let BREAK x = x |> BREAKK |> ignore

    let Console_WriteLine (txt: string) = Console.WriteLine(txt)

    let Console_WriteLineFlushed (txt: string) =
        let l = Console.CursorLeft
        let t = Console.CursorTop
        Console.WriteLine(new String(' ', Console.BufferWidth))
        Console.SetCursorPosition(l, t)
        Console.WriteLine(txt)

    module ConsoleUtils =
        let maybeGetKey() =
            if Console.KeyAvailable then
                Console.ReadKey(true) |> Some
            else None
        let mutable lastSize = None
        let setSize (size: int) =
            #if RELEASE
            let needsChange =
                match lastSize with
                | None -> true
                | Some lastSize when lastSize <> size -> true
                | _ -> false
            if needsChange then
                CLEAR $"Font size has changed from {lastSize} to {size}"
                //System.Diagnostics.Process.Start(@"C:\Users\Paul\AppData\Local\waveterm\Data\bin\wsh.EXE", $"""setmeta "term:fontsize={size}" """) |> ignore
                let psi = new System.Diagnostics.ProcessStartInfo(
                    @"C:\Users\Paul\AppData\Local\waveterm\Data\bin\wsh.EXE",
                    $"""setmeta "term:fontsize={size}" """
                )
                //psi.UseShellExecute <- true
                psi.RedirectStandardOutput <- true
                let processx = System.Diagnostics.Process.Start(psi)
                processx.WaitForExit() |> ignore
                //Console.WriteLine("Size is the same, no need to set it again.")
                System.Threading.Thread.Sleep(100) // Give it a moment to apply the size change
            #endif
            lastSize <- Some size
