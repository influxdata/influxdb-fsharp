[<NUnit.Framework.TestFixture>]
module InfluxDB.FSharp.IntegrationTests.Tests

open NUnit.Framework
open InfluxDB.FSharp

let (=?) (actual: 'a) (expected: 'a) =
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ``Ping``() =
    let client = Client("localhost")
    match client.Ping() with
    | Choice1Of2 (elapsed, version) ->
        printfn "ping elapsed: %O, version: %s" elapsed version
        version =? "0.9.1"
    | Choice2Of2 err -> failwithf "errror: %A" err
