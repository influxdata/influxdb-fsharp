[<NUnit.Framework.TestFixture>]
module InfluxDB.FSharp.IntegrationTests.Tests

open System
open System.Net
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open InfluxDB.FSharp
open InfluxDB.FSharp.UnitTests

// for running this tests you should have:
//   1. install Vagrant & VirtualBox
//   2. have ssh.exe in your %PATH% (from Windows Git distribution, for example)
//   3. run 'vagrant up' in src/InfluxDB.FSharp.IntegrationTests/ directory

// todo tests on user/passwd
// todo test: try create database that already exist
// todo tests on write errors
// todo tests on query errors

let integrationDbs = [|1..3|] |> Array.map (sprintf "IntegrationTest%d")

module Vagrant =
    let run command =
        let vagrantArgs = sprintf "ssh -c \"%s\"" command
        let stdout, stderr = Process.run "vagrant" vagrantArgs
        stringBuffer {
            if not (String.IsNullOrWhiteSpace stdout) then
                yield sprintf "vagrant stdout: %s" stdout
            if not (String.IsNullOrWhiteSpace stderr) then
                yield sprintf "vagrant stderr: %s" stderr
        } |> build |> printfn "%s"

module InfluxCLI =
    let run command =
        let influxCliCmd = sprintf "/opt/influxdb/influx -execute '%s'" command
        Vagrant.run influxCliCmd

type Generators =
    static member PointData() =
        Arb.Default.Derive() |> Arb.filter (tryCreateFrom >> Choice.isResult)

let run achoice =
    match Async.RunSynchronously achoice with
    | Ok x -> x
    | Fail e -> failwithf "Failed with: %A" e

let notFailA achoice =
    match Async.RunSynchronously achoice with
    | Ok x -> x
    | Fail e -> failwithf "Failed with: %A" e

let failA achoice =
    match Async.RunSynchronously achoice with
    | Ok x -> failwithf "Unexpectedly Ok with: %+A" x
    | Fail e -> e

let shouldNotFailA achoice = achoice |> notFailA |> ignore

let get = function Ok x -> x | Fail e -> failwithf "Unexpected Fail %+A" e

let machine = Environment.MachineName.ToLower()
let fiddler = { Address = "localhost"; Port = 8888us; Credentials = None }
let fmtTimestamp (value: DateTime) = value.ToString("yyyy-MM-ddTHH:mm:ssZ")

[<SetUp>]
let setup () =
    Vagrant.run "./dropalldb.sh"

[<Test>]
let Ping () =
    let client = Client(machine)
    let elapsed, version = run (client.Ping())
    printfn "ping elapsed: %O, version: %s" elapsed version
    Assert.That(version, Is.StringStarting("0.9.1"))

[<Test>]
let ShowDatabases () =
    let client = Client(machine)
    run (client.ShowDatabases()) =~? []

    shouldNotFailA (client.CreateDatabase(integrationDbs.[0]))
    run (client.ShowDatabases()) =~? [integrationDbs.[0]]

    shouldNotFailA (client.CreateDatabase(integrationDbs.[1]))
    run (client.ShowDatabases()) =~? integrationDbs.[0..1]

[<Test>]
let CreateDatabase () =
    let client = Client(machine)
    shouldNotFailA (client.CreateDatabase integrationDbs.[0])
    run (client.ShowDatabases()) =? [integrationDbs.[0]]

[<Test>]
let DropDatabase () =
    let client = Client(machine)
    shouldNotFailA (client.CreateDatabase integrationDbs.[0])
    run (client.ShowDatabases()) =? [integrationDbs.[0]]

    shouldNotFailA (client.DropDatabase integrationDbs.[0])
    run (client.ShowDatabases()) =? []

[<Test>]
let ``write point then query it back`` () =
    let db = integrationDbs.[0]
    let client = Client(machine)
    shouldNotFailA (client.CreateDatabase db)

    let timestamp = DateTime.UtcNow
    let internalVal = Int 32L
    let externalVal = Int 100L

    let data = { Measurement = "temperature"
                 Tags = Map [ "machine", "unit42"; "type", "assembly" ]
                 Fields = Map [ "internal", internalVal; "external", externalVal ]
                 Timestamp = Some timestamp }
    let point = createFrom data

    shouldNotFailA (client.Write(db, point, Precision.Seconds))

    let results = run (client.Query(db, "SELECT * FROM temperature"))
    match results with
    | result :: [] ->
        match result with
        | Ok series ->
            let serie = Seq.single series
            serie.Name =? data.Measurement
            serie.Tags =? data.Tags
            serie.Columns =? ["time"; "external"; "internal"]
            let values = Seq.single serie.Values
            values =? [| String (fmtTimestamp timestamp); externalVal; internalVal |]
        | Fail (ErrorMsg msg) -> failwithf "Query result error: %s" msg
    | x -> failwithf "unexpected results: %A" x

[<Property(Arbitrary=[|typeof<Generators>|])>]
[<Explicit>]
let ``write point then query it back [FsCheck]`` (data: PointData) =
    let db = integrationDbs.[0]
    let client = Client(machine, proxy = fiddler)
    shouldNotFailA (client.CreateDatabase db)

    let point = createFrom data
    shouldNotFailA (client.Write(db, point, Precision.Seconds))

    let results = run (client.Query(db, sprintf "SELECT * FROM %s" data.Measurement))
    match results with
    | result :: [] ->
        match result with
        | Ok series ->
            let serie = Seq.single series
            serie.Name =? data.Measurement
            serie.Tags =? data.Tags
            serie.Columns =? ["time"] @ (data.Fields |> Map.toList |> List.map fst)

            // todo check values
            //let values = Seq.single serie.Values
            //values =? [| String (fmtTimestamp timestamp); externalVal; internalVal |]
        | Fail (ErrorMsg msg) -> failwithf "Query result error: %s" msg
    | x -> failwithf "unexpected results: %A" x

[<Test>]
let ``query have wrong syntax => error in result`` () =
    let client = Client(machine, proxy = fiddler)
    let result = client.Query("nevermind", "SELECT wrong wrong wrong") |> Async.RunSynchronously

    match result with
    | Fail (HttpError (HttpStatusCode.BadRequest, Some msg)) -> msg =? "error parsing query: found wrong, expected FROM at line 1, char 14"
    | x -> failwithf "Unexpected result: %+A" x
