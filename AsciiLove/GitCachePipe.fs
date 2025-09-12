namespace AsciiLove
open System
open System.Diagnostics
open System.IO
open System.IO.Pipes
open System.Net.Http
open System.Text
open System.Text.Json
open System.Text.Json.Nodes

module GitStatusCacheClient =
    let mutable private pipe : NamedPipeClientStream option = None
    let private http = new HttpClient()

    let private getScriptDirectory () =
        let exePath =
            match Process.GetCurrentProcess().MainModule with
            | null -> AppContext.BaseDirectory
            | m -> m.FileName
        Path.GetDirectoryName(exePath)

    let private getBinPath () =
        Path.Combine(getScriptDirectory(), "bin")

    let private getExecutablePath () =
        Path.Combine(getBinPath(), "GitStatusCache.exe")

    let private removeGitStatusCache () =
        let procs = Process.GetProcessesByName("GitStatusCache")
        for p in procs do
            try p.Kill(true) with _ -> ()

        System.Threading.Thread.Sleep(50)

        let binPath = getBinPath()
        if Directory.Exists(binPath) then
            Directory.Delete(binPath, true)

    let private testRelease (release: JsonObject) =
        let tagName = release["tag_name"].ToString()
        if not (tagName.StartsWith("v1.")) then false
        else
            match release["assets"] with
            | null -> false
            | assets ->
                assets.AsArray()
                |> Seq.exists (fun a ->
                    let url = a.["browser_download_url"].ToString()
                    not (isNull url)
                    && url.EndsWith("GitStatusCache.exe", StringComparison.OrdinalIgnoreCase)
                )

    let getLatestReleaseJson () = task {
        use httpClient = new HttpClient()
        httpClient.DefaultRequestHeaders.Add("User-Agent", "YourAppName/1.0")
        let! latestJson =
            httpClient.GetStringAsync("https://api.github.com/repos/cmarcusreid/git-status-cache/releases/latest")
        return latestJson
    }

    let private getExecutableDownloadUrl (requireLatest: bool) = task {
        let! latestJson = getLatestReleaseJson()
        let mutable latest = JsonNode.Parse(latestJson).AsObject()

        if not (testRelease latest) then
            if requireLatest then failwith "Latest git-status-cache release is not compatible."

            http.DefaultRequestHeaders.UserAgent.ParseAdd("git-status-cache-posh-client")
            let! allReleasesJson =
                http.GetStringAsync("https://api.github.com/repos/cmarcusreid/git-status-cache/releases")

            let allReleases =
                JsonNode.Parse(allReleasesJson).AsArray()
                |> Seq.sortByDescending (fun x -> DateTime.Parse(x.["published_at"].ToString()))

            for r in allReleases do
                if testRelease (r.AsObject()) then
                    latest <- r.AsObject()
                    ()
        let q =
            latest["assets"].AsArray()
            |> Seq.map
                (fun asset ->
            //for asset in latest["assets"].AsArray() do
                    let url = asset.["browser_download_url"].ToString()
                    if url.EndsWith("GitStatusCache.exe", StringComparison.OrdinalIgnoreCase) then
                        Some url
                    else None
                )
            |> Seq.where (fun x -> x.IsSome)
            |> Seq.toList
            |> function
            | [] -> failwith "Failed to find GitStatusCache.exe download URL."
            | x :: _ -> x.Value

        return q

        //return raise (InvalidOperationException("Failed to find GitStatusCache.exe download URL."))
    }

    let updateGitStatusCache (requireLatest: bool) = task {
        removeGitStatusCache()
        let binPath = getBinPath()
        if not (Directory.Exists(binPath)) then
            Directory.CreateDirectory(binPath) |> ignore

        let exePath = getExecutablePath()
        if File.Exists(exePath) then File.Delete(exePath)

        let! url = getExecutableDownloadUrl requireLatest
        let! bytes = http.GetByteArrayAsync(url)
        do! File.WriteAllBytesAsync(exePath, bytes)
    }

    let private startGitStatusCache requireLatest =
        if Process.GetProcessesByName("GitStatusCache").Length = 0 then
            let exePath = getExecutablePath()
            if not (File.Exists(exePath)) then
                let t = updateGitStatusCache requireLatest
                t.Wait()
                //raise (InvalidOperationException("GitStatusCache.exe not found. Call UpdateGitStatusCache to download."))
            Process.Start(ProcessStartInfo(FileName = exePath)) |> ignore

    let private disconnectPipe () =
        match pipe with
        | Some p ->
            p.Dispose()
            pipe <- None
        | None -> ()

    let private connectPipe requireLatest =
        match pipe with
        | Some p when p.IsConnected -> ()
        | _ ->
            disconnectPipe()
            startGitStatusCache requireLatest
            let p = new NamedPipeClientStream(".", "GitStatusCache", PipeDirection.InOut, PipeOptions.WriteThrough)
            p.Connect(100)
            p.ReadMode <- PipeTransmissionMode.Message
            pipe <- Some p

    let private sendRequestToGitStatusCache requireLatest (requestJson: string) : JsonNode =
        connectPipe requireLatest
        let encoding = Encoding.UTF8
        let buffer = encoding.GetBytes(requestJson)

        let rec tryWrite (retries: int) : JsonNode =
            match pipe with
            | None -> raise (IOException "Pipe is not connected.")
            | Some (pipeClient: NamedPipeClientStream) ->
                try
                    let response =
                        pipeClient.Write(buffer, 0, buffer.Length)
                        let chunkSize = pipeClient.InBufferSize
                        let mutable totalBytesRead = 0
                        use memory = new MemoryStream()
                        let chunk = Array.zeroCreate<byte> chunkSize
                        while (
                            let bytesRead = pipeClient.Read(chunk, 0, chunkSize)
                            memory.Write(chunk, 0, bytesRead)
                            totalBytesRead <- totalBytesRead + bytesRead
                            bytesRead = chunkSize
                        ) do ()
                        encoding.GetString(memory.GetBuffer(), 0, totalBytesRead)

                    JsonNode.Parse(response)
                with :? IOException ->
                    disconnectPipe()
                    connectPipe requireLatest
                    if retries > 0 then
                        tryWrite (retries - 1)
                        |> fun x -> x
                    else
                        reraise()

        tryWrite 1

    let stopGitStatusCache requireLatest =
        let obj = {| Version = 1; Action = "Shutdown" |}
        let json = JsonSerializer.Serialize(obj)
        sendRequestToGitStatusCache requireLatest json

    let restartGitStatusCache requireLatest =
        stopGitStatusCache requireLatest |> ignore
        connectPipe requireLatest
        JsonObject()

    let getGitStatusFromCache requireLatest (path: string) =
        let obj = {| Version = 1; Action = "GetStatus"; Path = path |}
        let json = JsonSerializer.Serialize(obj)
        sendRequestToGitStatusCache requireLatest json

    let getGitStatusCacheStatistics requireLatest =
        let obj = {| Version = 1; Action = "GetCacheStatistics" |}
        let json = JsonSerializer.Serialize(obj)
        sendRequestToGitStatusCache requireLatest json