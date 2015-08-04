namespace InfluxDB.FSharp.IntegrationTests

open System
open System.Text
open System.Diagnostics
open InfluxDB.FSharp

[<AutoOpen>]
module Prelude =
    let (</>) path1 path2 = System.IO.Path.Combine (path1, path2)

module Process =
    let run workDir procName procArgs (timeout: TimeSpan) =
        let procInfo = ProcessStartInfo(procName, procArgs)
        procInfo.WorkingDirectory <- workDir
        procInfo.CreateNoWindow <- true
        procInfo.UseShellExecute <- false
        procInfo.RedirectStandardOutput <- true
        procInfo.RedirectStandardError <- true

        use proc = new Process()
        proc.StartInfo <- procInfo

        let stdout = StringBuilder()
        let stderr = StringBuilder()

        proc.OutputDataReceived.Add (fun args -> stdout.AppendLine args.Data |> ignore)
        proc.ErrorDataReceived.Add (fun args -> stderr.AppendLine args.Data |> ignore)

        if proc.Start() = false then
            failwithf "Cant start '%s %s'" procName procArgs

        do proc.BeginOutputReadLine()
        do proc.BeginErrorReadLine()

        if proc.WaitForExit(int timeout.TotalMilliseconds) then
            proc.ExitCode, stdout.ToString(), stderr.ToString()
        else
            do Choice.attempt proc.Kill |> ignore
            failwithf "Didnt wait of '%s %s'.\nstdout: %O\nstderr: %O" procName procArgs stdout stderr
