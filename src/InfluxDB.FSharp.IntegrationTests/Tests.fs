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

let run achoice =
    match Async.RunSynchronously achoice with
    | Ok x -> x
    | Fail e -> failwithf "Failed with: %A" e

let notFailed achoice =
    match Async.RunSynchronously achoice with
    | Ok () -> ()
    | Fail e -> failwithf "Failed with: %A" e

let machine = Environment.MachineName.ToLower()
let fiddler = { Address = "localhost"; Port = 8888us; Credentials = None }

[<SetUp>]
let setup () =
    // todo too slow, should think about this
    for db in integrationDbs do
        InfluxCLI.run (sprintf "DROP DATABASE %s" db)

[<Test>]
let Ping () =
    let client = Client("localhost")
    let elapsed, version = run (client.Ping())
    printfn "ping elapsed: %O, version: %s" elapsed version
    version =? "0.9.1"

[<Test>]
let ShowDatabases () =
    let client = Client("localhost")
    run (client.ShowDatabases()) =~? []

    notFailed (client.CreateDatabase(integrationDbs.[0]))
    run (client.ShowDatabases()) =~? [integrationDbs.[0]]

    notFailed (client.CreateDatabase(integrationDbs.[1]))
    run (client.ShowDatabases()) =~? integrationDbs.[0..1]

[<Test>]
let CreateDatabase () =
    let client = Client("localhost")
    notFailed (client.CreateDatabase integrationDbs.[0])
    run (client.ShowDatabases()) =? [integrationDbs.[0]]

[<Test>]
let DropDatabase () =
    let client = Client("localhost")
    notFailed (client.CreateDatabase integrationDbs.[0])
    run (client.ShowDatabases()) =? [integrationDbs.[0]]

    notFailed (client.DropDatabase integrationDbs.[0])
    run (client.ShowDatabases()) =? []

[<Test>]
let Write () =
    let db = integrationDbs.[0]
    let client = Client(machine, proxy = fiddler)
    notFailed (client.CreateDatabase db)

    let point = { Measurement = "temperature"
                  Tags = [ "machine", "unit42"; "type", "assembly" ] |> Map.ofList
                  Fields = [ "internal", Int32 32; "external", Int32 100 ] |> Map.ofList
                  Timestamp = DateTime.UtcNow }

    notFailed (client.Write(db, point, Precision.Seconds))
