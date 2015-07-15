namespace InfluxDB.FSharp

open System
open HttpClient
open InfluxDB.FSharp.AsyncChoice

type InfluxDbVersion = string

type PingError =
    | HttpError of exn
    | ResponseWithoutVersion

type Client (host: string, ?port: uint16, ?username: string, ?password: string) =
    let port = defaultArg port 8086us
    let baseUri = Uri(sprintf "http://%s:%d" host port)
    let url (path: string) = Uri(baseUri, path).AbsoluteUri

    let ping () = asyncChoice {
        let sw = Diagnostics.Stopwatch()
        do sw.Start()

        let! response =
            createRequest Get (url "ping")
            |> getResponseAsync
            |> Async.Catch
            <!!> HttpError
        do sw.Stop()

        let! version =
            response.Headers
            |> Map.tryFind (NonStandard "X-Influxdb-Version")
            |> Choice.ofOption
            |> Choice.mapFail (konst ResponseWithoutVersion)

        return sw.Elapsed, version
    }

    // todo duration param
    member __.PingAsync() : Async<Choice<TimeSpan*InfluxDbVersion, PingError>> = ping()
    member  x.Ping() : Choice<TimeSpan*InfluxDbVersion, PingError> = x.PingAsync() |> Async.RunSynchronously

