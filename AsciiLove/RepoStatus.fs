namespace AsciiLove
open System
open System.Text.Json.Nodes
open Atomic.Operators
open GitStatusCacheClient

open GitDomain
[<AutoOpen>]
module RepoStatus =
    let (|FitsType|_|) (json: string) : 't option =
        try
            Newtonsoft.Json.JsonConvert.DeserializeObject<'t>(json)
            |> Some
        with _ -> None
    let getGitStatusFromCacheStrong (path: string) =
        getGitStatusFromCache true path
        |> fun x ->
            let getArray (name: string) =
                x.[name] :?> JsonArray
                |> Seq.map
                    ^ fun node ->
                        node
                        |> function
                        | :? JsonValue as v -> v.ToString()
                        //| :? JsonObject as o ->
                        //    match o with
                        //    | FitsType (o: StashEntry) -> o.ToString() |> fun x -> x
                        //        //| (:? JsonObject) FitsType (o: GitStatus) ->  // Uncomment if you want to use this
                        //    | _ -> ""
                            //o.AsValue()
                        //| (:? JsonObject) FitsType (o: StashEntry) -> o.AsValue()
                        //| :? JsonArray as a -> a.AsValue()
                        //| :? JsonString as s -> s.AsValue()
                        | _ -> failwithf "Expected JsonValue, got %A" node
                        |> fun x -> x
                        //|> ignore

                        //node.GetValue<string>()
                |> Seq.toList
            let v =
                    let xx = x.["Version"]
                    try
                        xx.ToString() |> int
                    with failed ->
                        BREAK xx
                        //failwith "asd"
                        0
            let s =
                let ss = x.["Stashes"]
                ss.AsArray()
                |> Seq.map
                    ^ fun x ->
                        //x.GetValue<StashEntry>()
                        match x.ToJsonString() with
                        | FitsType (o: StashEntry) -> o
                        | _ -> failwithf "Expected StashEntry, got %A" x
                |> Seq.toList
                //ss.GetValue<StashEntry>()
            { GitDomain.GitRepoStatusReport.Empty with
                Version           = v
                //Op: Auto
                Path            = x.["Path"].ToString()
                RepoPath        = x.["RepoPath"].ToString()
                WorkingDir      = x.["WorkingDir"].ToString()
                State           = x.["State"].ToString()
                Branch          = x.["Branch"].ToString()
                Upstream        = x.["Upstream"].ToString()
                UpstreamGone    = x.["UpstreamGone"].GetValue<bool>()
                AheadBy         = x.["AheadBy"].GetValue<int>()
                BehindBy        = x.["BehindBy"].GetValue<int>()
                //IndexAdded    = getArray "IndexAdded"
                //IndexModified = getArray "IndexModified"
                //Ignored       = getArray "Ignored"
                //Conflicted    = getArray "Conflicted"
                Stashes         = s
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
                //IndexDeleted      = getArray "IndexDeleted"
                //IndexTypeChange   = getArray "IndexTypeChange"
                //IndexRenamed      = getArray "IndexRenamed"
                //WorkingAdded      = getArray "WorkingAdded"
                //WorkingModified   = getArray "WorkingModified"
                //WorkingDeleted    = getArray "WorkingDeleted"
                //WorkingTypeChange = getArray "WorkingTypeChange"
                //WorkingRenamed    = getArray "WorkingRenamed"
                //WorkingUnreadable = getArray "WorkingUnreadable"
            }

    let getDirtFromStatus (s: GitRepoStatusReport) =
        let aheadBy = s.AheadBy
        let behindBy = s.BehindBy

        let stashCount = s.Stashes.Length
        let aheadBehindStashDirtCount = aheadBy + behindBy + stashCount

        let dirt =
            [
                if aheadBy > 0 then $"Ahead: %i{aheadBy}"
                if behindBy > 0 then $"Behind: %i{behindBy}"
                if s.Stashes.Length > 0 then $"Stashes: %i{s.Stashes.Length}"
            ]
            |> fun generalDirt ->
                s.FileBuckets
                |> Seq.choose ^ fun x -> if x.Files.Length > 0 then $"{x.Name}: %i{x.Files.Length}" |> Some  else None
                |> Seq.toList
                |> fun specificTypesOfDirt ->
                    generalDirt @ specificTypesOfDirt

        let dirtyFiles = s.FileBuckets |> Seq.sumBy _.Files.Length

        (dirtyFiles > 0 || aheadBehindStashDirtCount > 0), dirt

    let printGitRepoStatusReport (side: CursorSide) (repoSelected: bool) (selectedFile: string option) (s: GitRepoStatusReport) (meta: GitRepoStatusReportMeta) (dirt: string list) =
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
        writeWithColor ConsoleColor.White $"{repoSelected}"
        writeWithColor ConsoleColor.Green $@"{s.Path}\ "
        writeWithColor ConsoleColor.White $@"[{s.Branch}] "
        writeWithColor ConsoleColor.Blue $"[{dirtText}]"
        //writeWithColor ConsoleColor.Blue $" [{meta.Visibility}]"
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
                writeWithColor ConsoleColor.Yellow $" {fileBucket.name}: {fileBucket.filesInBucket.Length} files"
                writeLine ""
                fileBucket.filesInBucket
                |% fun fileInBucket ->
                    let isSelected = selectedFile.IsSome && selectedFile.Value = fileInBucket
                    if isSelected && side = CursorSide.Right then
                        "->"
                    else
                        "  "
                    |> writeWithColor ConsoleColor.White

                    writeWithColor fileBucket.consoleColor $"  {fileInBucket}"
                    writeLine ""

        Console.ResetColor()