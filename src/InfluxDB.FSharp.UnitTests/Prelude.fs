[<NUnit.Framework.TestFixture>]
module InfluxDB.FSharp.UnitTests.Prelude

open NUnit.Framework
open InfluxDB.FSharp

[<Test>]
let ``Seq.trySingle`` () =
    Seq.trySingle [1] =? Some 1
    Seq.trySingle [] =? None
    Seq.trySingle [1; 2] =? None
