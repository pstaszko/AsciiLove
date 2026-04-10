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
