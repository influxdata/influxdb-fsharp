[<NUnit.Framework.TestFixture>]
module InfluxDB.FSharp.UnitTests.Prelude

open System
open NUnit.Framework
open InfluxDB.FSharp

[<Test>]
let ``Seq.trySingle`` () =
    Seq.trySingle [1] =? Some 1
    Seq.trySingle [] =? None
    Seq.trySingle [1; 2] =? None

let unixEpochStart = DateTime(1970, 1, 1, 0, 0, 0, 0, DateTimeKind.Utc)

[<Test>]
let ``DateTime.toUnixNanoseconds`` () =
    DateTime.toUnixNanoseconds (unixEpochStart.AddMilliseconds 1.) =? uint64 1e6

[<Test>]
let ``DateTime.toUnixMicroseconds`` () =
    DateTime.toUnixMicroseconds (unixEpochStart.AddMilliseconds 1.) =? uint64 1e3

[<Test>]
let ``DateTime.toUnixMilliseconds`` () =
    DateTime.toUnixMilliseconds (unixEpochStart.AddMilliseconds 1.) =? 1uL

[<Test>]
let ``DateTime.toUnixSeconds`` () =
    DateTime.toUnixSeconds (unixEpochStart.AddSeconds 1.) =? 1uL

[<Test>]
let ``DateTime.toUnixMinutes`` () =
    DateTime.toUnixMinutes (unixEpochStart.AddMinutes 1.) =? 1uL

[<Test>]
let ``DateTime.toUnixHours`` () =
    DateTime.toUnixHours (unixEpochStart.AddHours 1.) =? 1uL
