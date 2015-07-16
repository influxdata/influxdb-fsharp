namespace InfluxDB.FSharp

open System
open System.Net
open HttpClient
open InfluxDB.FSharp.Choice
open InfluxDB.FSharp.AsyncChoice

module internal Contracts =
    open System.Runtime.Serialization

    module ShowDatabases =
        [<DataContract>]
        type Series =
            {
                [<field: DataMember(Name="name")>]
                Name: string

                [<field: DataMember(Name="columns")>]
                Columns: string[]

                [<field: DataMember(Name="values")>]
                Values: string[][]
            }

        [<DataContract>]
        type Results =
            {
                [<field: DataMember(Name="series")>]
                Series: Series[]
            }

        [<DataContract>]
        type Response =
            {
                [<field: DataMember(Name="results")>]
                Results: Results[]
            }

    let deserialize<'a> (json: string) =
        Choice.attempt <| fun () ->
            use ms = new IO.MemoryStream(Text.Encoding.Unicode.GetBytes json)
            let serializer = System.Runtime.Serialization.Json.DataContractJsonSerializer(typedefof<'a>)
            serializer.ReadObject(ms) :?> 'a

type InfluxDbVersion = string

type Error =
    | TransportError of exn
    | HttpError of HttpStatusCode
    | ResponseParseError
    | OtherError of string

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
            <!!> TransportError
        do sw.Stop()

        let! version =
            response.Headers
            |> Map.tryFind (NonStandard "X-Influxdb-Version")
            |> function
            | Some version -> Ok version
            | None -> Fail (OtherError "No version header in response")

        return sw.Elapsed, version
    }

    let createDb name = asyncChoice {
        let! response =
            createRequest Get (url "query")
            |> withQueryStringItem { name="q"; value=sprintf "CREATE DATABASE %s" name }
            |> getResponseAsync
            |> Async.Catch
            <!!> TransportError

        return!
            match enum response.StatusCode with
            | HttpStatusCode.OK -> Ok ()
            | code -> Fail (HttpError code)
    }

    let showDbs () = asyncChoice {
        let! response =
            createRequest Get (url "query")
            |> withQueryStringItem { name="q"; value="SHOW DATABASES" }
            |> getResponseAsync
            |> Async.Catch
            <!!> TransportError

        return!
            match enum response.StatusCode with
            | HttpStatusCode.OK ->
                choice {
                    let! json = response.EntityBody |> Choice.ofOption |> Choice.mapFail (fun _ -> OtherError "Response doesnt contain body")
                    let! response = Contracts.deserialize<Contracts.ShowDatabases.Response> json <!~> ResponseParseError
                    return!
                        response.Results
                        |> Seq.trySingle
                        |> Option.bind (fun r -> Seq.trySingle r.Series)
                        |> Choice.ofOption
                        |> Choice.mapFail (konst ResponseParseError)
                        |> function
                           | Ok series ->
                                match Option.ofNull series.Values with
                                | Some values ->
                                    Ok (values |> Seq.collect id |> Seq.toList)
                                | None -> Ok []
                            | Fail e -> Fail e
                }
            | code -> Fail (HttpError code)
    }

    // todo duration param as in Go client?
    member __.PingAsync() = ping()
    member  x.Ping() = x.PingAsync() |> Async.RunSynchronously

    member __.CreateDatabaseAsync(name: string) = createDb name
    member  x.CreateDatabase(name: string) = x.CreateDatabaseAsync name |> Async.RunSynchronously

    member __.ShowDatabasesAsync() = showDbs()
    member  x.ShowDatabases() = x.ShowDatabasesAsync() |> Async.RunSynchronously
