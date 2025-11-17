namespace AsciiLove
//Op: Auto
open System
//Op: End
[<AutoOpen>]
module GitDomain =
    type StashEntry =
        {
            Name: string
            Sha1Id: string
            Message: string
        }

    type Visibility =
        | Visible
        | Hidden
        | Collapsed

    type GitStatusMetaKey =
        {
            Path: string
        }

    type GitRepoStatusReportMeta =
        {
            Visibility: Visibility
            //SelectedFile: string option
        }
        static member Default =
            {
                Visibility = Visible
                //SelectedFile = None
            }
    type FileBucket =
        {
            Name: string
            Color: ConsoleColor
            Files: string list
        }
    type GitRepoStatusReport = {
        Version: int
        Path: string
        [<Obsolete>]
        RepoPath: string
        WorkingDir: string
        State: string
        Branch: string
        Upstream: string
        UpstreamGone: bool
        AheadBy: int
        BehindBy: int
        FileBuckets: FileBucket list
        //IndexAdded: string list
        //IndexModified: string list
        //IndexDeleted: string list
        //IndexTypeChange: string list
        //IndexRenamed: string list
        //WorkingAdded: string list
        //WorkingModified: string list
        //WorkingDeleted: string list
        //WorkingTypeChange: string list
        //WorkingRenamed: string list
        //WorkingUnreadable: string list
        Ignored: string list
        Conflicted: string list
        Stashes: StashEntry list
    } with
        static member Empty =
            {
                Version = 0
                Path = ""
                RepoPath = ""
                WorkingDir = ""
                State = ""
                Branch = ""
                Upstream = ""
                UpstreamGone = false
                AheadBy = 0
                BehindBy = 0
                FileBuckets = []
                //IndexDeleted = []
                //IndexTypeChange = []
                //IndexRenamed = []
                //WorkingAdded = []
                //WorkingModified = []
                //WorkingDeleted = []
                //WorkingTypeChange = []
                //WorkingRenamed = []
                //WorkingUnreadable = []
                Ignored = []
                Conflicted = []
                Stashes = []
            }
        member this.GetGitStatusMetaKey () : GitStatusMetaKey =
            { Path = this.Path }

    let isGitRepository (path: string) : bool =
        let gitDir = System.IO.Path.Combine(path, ".git")
        System.IO.Directory.Exists(gitDir)
        
    let assertIsGitRepoDir(path: string) : unit =
        if System.IO.Directory.Exists(path) then
            if isGitRepository path then
                ()
            else
                failwith $"The directory '{path}' is not a git repository."
        else
            raise (System.IO.DirectoryNotFoundException($"The directory '{path}' does not exist."))
