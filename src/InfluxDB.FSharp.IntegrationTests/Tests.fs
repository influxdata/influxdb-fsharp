[<NUnit.Framework.TestFixture>]
module InfluxDB.FSharp.IntegrationTests.Tests

open System
open System.Net
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open InfluxDB.FSharp
open InfluxDB.FSharp.UnitTests

// for running this tests you should:
//   1. install Vagrant & VirtualBox
//   2. have ssh.exe in your %PATH% (from Windows Git distribution, for example)

// todo tests on user/passwd
// todo tests on write errors

let integrationDbs = [|1..3|] |> Array.map (sprintf "IntegrationTest%d")

module Vagrant =
    let private run = Process.run "../../" "vagrant"

    let up () =
        match run "up" (TimeSpan.FromMinutes 3.) with
        | 0, _, _ -> ()
        | x, out, err -> failwithf "vagrant up exit with %d\nstdout: %s\nstderr: %s" x out err

    let destroy () =
        run "destroy -f" (TimeSpan.FromSeconds 30.) |> ignore

    let exec command =
        let vagrantArgs = sprintf "ssh -c \"%s\"" command
        let _, stdout, stderr = run vagrantArgs (TimeSpan.FromSeconds 20.)
        stringBuffer {
            if not (String.IsNullOrWhiteSpace stdout) then
                yield sprintf "vagrant stdout: %s" stdout
            if not (String.IsNullOrWhiteSpace stderr) then
                yield sprintf "vagrant stderr: %s" stderr
        } |> build |> printfn "%s"

module InfluxCLI =
    let run command =
        let influxCliCmd = sprintf "/opt/influxdb/influx -execute '%s'" command
        Vagrant.exec influxCliCmd

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
let fiddler = { Address = "localhost"; Port = 8888us; Credentials = ProxyCredentials.No }
let fmtTimestamp (value: DateTime) = value.ToString("yyyy-MM-ddTHH:mm:ssZ")

[<TestFixtureSetUp>]
let fixtureSetup() = Vagrant.up()

//[<TestFixtureTearDown>]
//let fixtureTeardown() = Vagrant.destroy()

[<SetUp>]
let setup () = Vagrant.exec "./dropalldb.sh"

[<Test>]
let ping () =
    let client = Client(machine)
    let elapsed, version = run (client.Ping())
    printfn "ping elapsed: %O, version: %s" elapsed version
    Assert.That(version, Is.StringStarting "0.9")

[<Test>]
let ``show databases`` () =
    let client = Client(machine)
    run (client.ShowDatabases()) =~? []

    shouldNotFailA (client.CreateDatabase(integrationDbs.[0]))
    run (client.ShowDatabases()) =~? [integrationDbs.[0]]

    shouldNotFailA (client.CreateDatabase(integrationDbs.[1]))
    run (client.ShowDatabases()) =~? integrationDbs.[0..1]

[<Test>]
let ``create database`` () =
    let client = Client(machine)
    shouldNotFailA (client.CreateDatabase integrationDbs.[0])
    run (client.ShowDatabases()) =? [integrationDbs.[0]]

[<Test>]
let ``try create database that already exist => error`` () =
    let client = Client(machine)
    let db = integrationDbs.[0]
    shouldNotFailA (client.CreateDatabase db)

    match client.CreateDatabase db |> Async.RunSynchronously with
    | Fail (ServerError "database already exists") -> ()
    | x -> failwithf "Unexpected result: %+A" x

[<Test>]
let ``drop of exiting database`` () =
    let client = Client(machine)
    shouldNotFailA (client.CreateDatabase integrationDbs.[0])
    run (client.ShowDatabases()) =? [integrationDbs.[0]]

    shouldNotFailA (client.DropDatabase integrationDbs.[0])
    run (client.ShowDatabases()) =? []

[<Test>]
let ``try drop not existing database => error`` () =
    let client = Client(machine)
    run (client.ShowDatabases()) =? []

    match Async.RunSynchronously (client.DropDatabase "not_exist") with
    | Ok () -> failwithf "Unexpectedly Ok"
    | Fail (ServerError msg) -> msg =? "database not found: not_exist"
    | Fail x -> failwithf "Wrong Error %+A" x

[<Test>]
let ``write point then query it back`` () =
    let db = integrationDbs.[0]
    let client = Client(machine)
    shouldNotFailA (client.CreateDatabase db)

    let timestamp = DateTime.UtcNow
    let internalVal = Int 32L
    let externalVal = Int 100L
    let machine = String "unit42"
    let typ = String "assembly"

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
            serie.Tags =? Map.empty     // query without GROUP BY, all tags transformed into values
            serie.Columns =? ["time"; "external"; "internal"; "machine"; "type"]
            let values = Seq.single serie.Values
            values =? [| String (fmtTimestamp timestamp); externalVal; internalVal; machine; typ |]
        | Fail msg -> failwithf "Query result error: %s" msg
    | x -> failwithf "unexpected results: %A" x

[<Explicit>]
[<Property(Arbitrary=[|typeof<Generators>|])>]
// this test generates some data that influxdb 0.9.2 start hangs
// I should try it on later versions and report possible issues
let ``write point then query it back [FsCheck]`` (data: PointData) =
    let db = integrationDbs.[0]
    let client = Client(machine, proxy = fiddler)
    //client.DropDatabase db
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
        | Fail msg -> failwithf "Query result error: %s" msg
    | x -> failwithf "unexpected results: %A" x

[<Test>]
let ``write many points then query it back`` () =
    let db = integrationDbs.[0]
    let client = Client(machine)
    shouldNotFailA (client.CreateDatabase db)

    let timestamp = DateTime.UtcNow
    let temperatureInternal = Int 1L
    let temperatureExternal = Int 100L
    let machine = "unit42"
    let typ = "assembly"

    let cpuLoad = Float 0.64
    let host = "server01"
    let region = "us-west"

    let temperatureData = { Measurement = "temperature"
                            Tags = Map [ "machine", machine; "type", typ ]
                            Fields = Map [ "internal", temperatureInternal; "external", temperatureExternal ]
                            Timestamp = Some timestamp }
    let cpuData = { Measurement = "cpu_load_short"
                    Tags = Map [ "host", host; "region", region ]
                    Fields = Map [ "value", cpuLoad ]
                    Timestamp = Some timestamp }
    let points = createFromMany [| temperatureData; cpuData |]

    shouldNotFailA (client.WriteMany(db, points, Precision.Seconds))

    let querySingle query =
        let results = run (client.Query(db, query))
        match results with
        | result :: [] ->
            match result with
            | Ok series -> Seq.single series
            | Fail msg -> failwithf "Query result error: %s" msg
        | x -> failwithf "unexpected results: %A" x

    // check temperature
    let temperatureSerie = querySingle "SELECT * FROM temperature"
    temperatureSerie.Name =? temperatureData.Measurement
    temperatureSerie.Tags =? Map.empty
    temperatureSerie.Columns =? ["time"; "external"; "internal"; "machine"; "type"]
    let temperatureValues = Seq.single temperatureSerie.Values
    temperatureValues =? [| String (fmtTimestamp timestamp); temperatureExternal; temperatureInternal; String machine; String typ |]

    // check cpu_load_short
    let cpuSerie = querySingle "SELECT * FROM cpu_load_short"
    cpuSerie.Name =? cpuData.Measurement
    cpuSerie.Tags =? Map.empty
    cpuSerie.Columns =? ["time"; "host"; "region"; "value"]
    let cpuValues = Seq.single cpuSerie.Values
    cpuValues =? [| String (fmtTimestamp timestamp); String host; String region; cpuLoad |]

[<Test>]
let ``query with GROUP BY tags => tags appears in result tags property, but not in values``() =
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

    let results = run (client.Query(db, "SELECT * FROM temperature GROUP BY machine,type"))
    match results with
    | result :: [] ->
        match result with
        | Ok series ->
            let serie = Seq.single series
            serie.Tags =? data.Tags
            serie.Columns =? ["time"; "external"; "internal"]
            let values = Seq.single serie.Values
            values =? [| String (fmtTimestamp timestamp); externalVal; internalVal |]
        | Fail msg -> failwithf "Query result error: %s" msg
    | x -> failwithf "unexpected results: %A" x

[<Test>]
let ``query have wrong syntax => error in response`` () =
    let client = Client(machine)
    let result = client.Query("nevermind", "SELECT wrong wrong wrong") |> Async.RunSynchronously

    match result with
    | Fail (HttpError (BadStatusCode (HttpStatusCode.BadRequest, Some msg))) -> msg =? "error parsing query: found wrong, expected FROM at line 1, char 14"
    | x -> failwithf "Unexpected result: %+A" x

[<Test>]
let ``query not existing db => error`` () =
    let client = Client(machine)
    let results = notFailA (client.Query("not_exist", "SELECT * FROM nevermind"))

    match results with
    | [ Fail msg ] -> msg =? "database not found: not_exist"
    | x -> failwithf "Unexpected result: %+A" x

// https://github.com/influxdb/influxdb.com/issues/137
[<Test>]
let ``query not exist serie => empty series list in response`` () =
    let client = Client(machine)
    let db = integrationDbs.[0]
    shouldNotFailA (client.CreateDatabase(db))

    let result = client.Query(db, "SELECT * FROM notexistserie") |> Async.RunSynchronously

    match result with
    | Ok result -> result =? [ Ok [] ]
    | x -> failwithf "Unexpected result: %+A" x
