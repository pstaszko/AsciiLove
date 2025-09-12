namespace AsciiLove
open System
[<AutoOpen>]
module Global =
    let swap fn arg1 arg2 = fn arg2 arg1

    let CLEAR (reason: string) =
        Logging.logVerbose $"RealClear: {reason}"
        Console.Clear()
        Console.WriteLine("\x1b[3J")
        Console.Clear()

    let BREAKK x = x

    let BREAK x = x |> BREAKK |> ignore

    let Console_WriteLine (txt: string) = Console.WriteLine(txt)

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
