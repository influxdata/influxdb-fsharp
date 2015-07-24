namespace InfluxDB.FSharp.IntegrationTests

open System
open System.Text
open System.Diagnostics
open InfluxDB.FSharp

[<AutoOpen>]
module Prelude =
    let (</>) path1 path2 = System.IO.Path.Combine (path1, path2)

module Process =
    let run procName procArgs =
        let procInfo = ProcessStartInfo(procName, procArgs)
        procInfo.CreateNoWindow <- true
        procInfo.UseShellExecute <- false
        procInfo.RedirectStandardOutput <- true
        procInfo.RedirectStandardError <- true

        use proc = new Process()
        proc.StartInfo <- procInfo

        let stdout = StringBuilder()
        let stderr = StringBuilder()

        proc.OutputDataReceived.Add (fun args -> stdout.Append args.Data |> ignore)
        proc.ErrorDataReceived.Add (fun args -> stderr.Append args.Data |> ignore)

        if proc.Start() = false then
            failwithf "Cant start '%s %s'" procName procArgs

        do proc.BeginOutputReadLine()
        do proc.BeginErrorReadLine()

        if proc.WaitForExit(int (TimeSpan.FromSeconds(20.).TotalMilliseconds)) then
            stdout.ToString(), stderr.ToString()
        else
            do Choice.attempt proc.Kill |> ignore
            failwithf "Didnt wait of '%s %s'.\nstdout: %O\nstderr: %O" procName procArgs stdout stderr
