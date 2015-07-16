[<NUnit.Framework.TestFixture>]
module InfluxDB.FSharp.IntegrationTests.Tests

open System
open NUnit.Framework
open InfluxDB.FSharp
open InfluxDB.FSharp.UnitTests

// for running this tests you should have:
//   1. install Vagrant & VirtualBox
//   2. have ssh.exe in your %PATH% (from Windows Git distribution, for example)
//   3. run  'vagrant up' in InfluxDB.FSharp.IntegrationTests directory

// todo tests on user/passwd
// todo test: try create database that already exist

let integrationDbs = [|1..3|] |> Array.map (sprintf "IntegrationTest%d")

module InfluxCLI =
    let run command =
        let influxCliCmd = sprintf "/opt/influxdb/influx -execute '%s'" command
        let vagrantArgs = sprintf "ssh -c \"%s\"" influxCliCmd
        let stdout, stderr = Process.run "vagrant" vagrantArgs

        stringBuffer {
            if not (String.IsNullOrWhiteSpace stdout) then
                yield sprintf "vagrant stdout: %s" stdout
            if not (String.IsNullOrWhiteSpace stderr) then
                yield sprintf "vagrant stderr: %s" stderr
        } |> build |> printfn "%s"

let get =
    function
    | Ok x -> x
    | Fail e -> failwithf "Failed with: %A" e

let notFailed =
    function
    | Ok () -> ()
    | Fail e -> failwithf "Failed with: %A" e

[<SetUp>]
let setup () =
    // todo too slow, should think about this
    for db in integrationDbs do
        InfluxCLI.run (sprintf "DROP DATABASE %s" db)

[<Test>]
let Ping () =
    let client = Client("localhost")
    match client.Ping() with
    | Choice1Of2 (elapsed, version) ->
        printfn "ping elapsed: %O, version: %s" elapsed version
        version =? "0.9.1"
    | Choice2Of2 err -> failwithf "errror: %A" err

[<Test>]
let CreateDatabase () =
    let client = Client("localhost")
    notFailed (client.CreateDatabase integrationDbs.[0])
    get (client.ShowDatabases()) =? [integrationDbs.[0]]

[<Test>]
let ShowDatabases () =
    let client = Client("localhost")
    get (client.ShowDatabases()) =~? []

    notFailed (client.CreateDatabase(integrationDbs.[0]))
    get (client.ShowDatabases()) =~? [integrationDbs.[0]]

    notFailed (client.CreateDatabase(integrationDbs.[1]))
    get (client.ShowDatabases()) =~? integrationDbs.[0..1]
