namespace AsciiLove.GitStatus
//Op: Auto
open System
open System.Text.Json.Nodes

open Atomic.Operators
open git_status_cache_dotnet.Client

open GitDomain
open AsciiLove
//Op: End
[<AutoOpen>]
module RepoStatus =
    let (|FitsType|_|) (json: string) : 't option =
        try
            Newtonsoft.Json.JsonConvert.DeserializeObject<'t>(json)
            |> Some
        with _ -> None

    let getGitStatusFromCacheStrong (path: string) =
        getGitStatusFromCache true path
        |> fun jsonNode ->
            let getArray (name: string) : SelectableFile list =
                jsonNode.[name] :?> JsonArray
                |> Seq.map
                    ^ fun node ->
                        node
                        |> function
                        | :? JsonValue as v -> v.ToString()
                        | _ -> failwithf "Expected JsonValue, got %A" node
                        |> fun x -> x
                |> Seq.toList
                |+| fun x ->
                    {
                        RepoPath = path
                        FilePath = x
                    }

            let version =
                let versionNode = jsonNode.["Version"]
                try
                    versionNode.ToString() |> int
                with failed ->
                    BREAK versionNode
                    0

            let s =
                let ss = jsonNode.["Stashes"]
                ss.AsArray()
                |> Seq.map
                    ^ fun x ->
                        match x.ToJsonString() with
                        | FitsType (o: StashEntry) -> o
                        | _ -> failwithf "Expected StashEntry, got %A" x
                |> Seq.toList

            { GitDomain.GitRepoStatusReport.Empty with
                //Op: CollapseWhitespaceKeepIndent
                //Op: Auto
                Version      = version
                Path         = jsonNode.["Path"].ToString()
                RepoPath     = jsonNode.["RepoPath"].ToString()
                WorkingDir   = jsonNode.["WorkingDir"].ToString()
                State        = jsonNode.["State"].ToString()
                Branch       = jsonNode.["Branch"].ToString()
                Upstream     = jsonNode.["Upstream"].ToString()
                UpstreamGone = jsonNode.["UpstreamGone"].GetValue<bool>()
                AheadBy      = jsonNode.["AheadBy"].GetValue<int>()
                BehindBy     = jsonNode.["BehindBy"].GetValue<int>()
                Stashes      = s
                //Op: End
                FileBuckets =
                    [
                        //Op: Auto
                        //Op: CollapseWhitespaceKeepIndent
                        //Op: IndentOn ;
                        //Op: IndentOn }
                        { FileBucket.Name = "Ignored"          ; Color = ConsoleColor.DarkGray; Files = getArray "Ignored"           }
                        { FileBucket.Name = "Conflicted"       ; Color = ConsoleColor.Yellow  ; Files = getArray "Conflicted"        }
                        { FileBucket.Name = "IndexAdded"       ; Color = ConsoleColor.Green   ; Files = getArray "IndexAdded"        }
                        { FileBucket.Name = "IndexModified"    ; Color = ConsoleColor.Green   ; Files = getArray "IndexModified"     }
                        { FileBucket.Name = "IndexDeleted"     ; Color = ConsoleColor.Green   ; Files = getArray "IndexDeleted"      }
                        { FileBucket.Name = "IndexTypeChange"  ; Color = ConsoleColor.Green   ; Files = getArray "IndexTypeChange"   }
                        { FileBucket.Name = "IndexRenamed"     ; Color = ConsoleColor.Green   ; Files = getArray "IndexRenamed"      }
                        { FileBucket.Name = "WorkingAdded"     ; Color = ConsoleColor.Red     ; Files = getArray "WorkingAdded"      }
                        { FileBucket.Name = "WorkingModified"  ; Color = ConsoleColor.Red     ; Files = getArray "WorkingModified"   }
                        { FileBucket.Name = "WorkingDeleted"   ; Color = ConsoleColor.Red     ; Files = getArray "WorkingDeleted"    }
                        { FileBucket.Name = "WorkingTypeChange"; Color = ConsoleColor.Red     ; Files = getArray "WorkingTypeChange" }
                        { FileBucket.Name = "WorkingRenamed"   ; Color = ConsoleColor.Red     ; Files = getArray "WorkingRenamed"    }
                        { FileBucket.Name = "WorkingUnreadable"; Color = ConsoleColor.Red     ; Files = getArray "WorkingUnreadable" }
                        //Op: End
                    ]
            }

    let getDirtFromStatus (s: GitRepoStatusReport) =
        let aheadBy = s.AheadBy
        let behindBy = s.BehindBy

        let stashCount = s.Stashes.Length
        let aheadBehindStashDirtCount = aheadBy + behindBy + stashCount

        let dirtText =
            [
                if aheadBy > 0 then $"Ahead: %i{aheadBy}"
                if behindBy > 0 then $"Behind: %i{behindBy}"
                if s.Stashes.Length > 0 then $"Stashes: %i{s.Stashes.Length}"
            ]
            |> fun generalDirt ->
                s.FileBuckets
                |> Seq.choose ^ fun x -> if x.Files.Length > 0 then $"%s{x.Name}: %i{x.Files.Length}" |> Some  else None
                |> Seq.toList
                |> fun specificTypesOfDirt ->
                    generalDirt @ specificTypesOfDirt

        let dirtyFiles = s.FileBuckets |> Seq.sumBy _.Files.Length

        (dirtyFiles > 0 || aheadBehindStashDirtCount > 0), dirtText

    let printGitRepoStatusReport selectedFileSymbol (side: CursorSide) (repoSelected: bool) (selectedFile: SelectableFile option) (s: GitRepoStatusReport) (meta: GitRepoStatusReportMeta) (dirt: string list) =
        let indentation = "    "
        let repoSelected =
            if repoSelected && side = CursorSide.Left then "-> "
            else "   "
        let writeWithColor color (text: string) =
            Console.ForegroundColor <- color
            Console.Write(text)

        let writeLine (text: string) =
            Console_WriteLine text
            Console.Write indentation

        let dirtText = String.Join(", ", dirt)
        writeWithColor ConsoleColor.White $"%s{repoSelected}"
        writeWithColor ConsoleColor.Green $@"%s{s.Path}\ "
        writeWithColor ConsoleColor.White $@"[%s{s.Branch}] "
        writeWithColor ConsoleColor.Blue $"[%s{dirtText}]"
        writeLine ""

        let filesBucketsToNote =
            s.FileBuckets
            |+| fun x ->
                {|
                    consoleColor = x.Color
                    name = x.Name
                    filesInBucket = x.Files
                |}

        if meta.Visibility = Visibility.Visible then
            filesBucketsToNote
            |?! _.filesInBucket.IsEmpty
            |% fun fileBucket ->
                writeWithColor ConsoleColor.Yellow $" %s{fileBucket.name}: %i{fileBucket.filesInBucket.Length} files"
                writeLine ""
                fileBucket.filesInBucket
                |% fun fileInBucket ->
                    let isSelected = selectedFile.IsSome && selectedFile.Value.FullPath = fileInBucket.FullPath
                    if isSelected && side = CursorSide.Right then
                        selectedFileSymbol
                    else
                        new String(' ', selectedFileSymbol.Length)
                        //"  "
                    |> writeWithColor ConsoleColor.White

                    writeWithColor fileBucket.consoleColor $"  %s{fileInBucket.FilePath}"
                    writeLine ""

        Console.ResetColor()