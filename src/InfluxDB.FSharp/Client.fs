﻿namespace InfluxDB.FSharp

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

type FieldValue =
    | Int32 of int
    | Int64 of int64
    | Double of double
    | String of string
    | Bool of bool
    | Bytes of byte[]

type Point =
    { Measurement: string
      Tags: Map<string,string>
      Fields: Map<string, FieldValue>
      Timestamp: DateTime }

type Precision =
    | Nanoseconds = 0
    | Microseconds = 1
    | Milliseconds = 2
    | Seconds = 3
    | Minutes = 4
    | Hours = 5

type Database = string
type InfluxDbVersion = string

type Error =
    | TransportError of exn
    | HttpError of code: HttpStatusCode * msg: string option
    | ResponseParseError
    | OtherError of string

type Credentials =
    { Username: string
      Password: string }

type Proxy =
    { Address: string
      Port: uint16
      Credentials: Credentials option }

// todo xml docs on public members
// todo validate host
// todo validate proxy
type Client (host: string, ?port: uint16, ?credentials: Credentials, ?proxy: Proxy) =
    let port = defaultArg port 8086us
    let baseUri = Uri(sprintf "http://%s:%d" host port)
    let url (path: string) = Uri(baseUri, path).AbsoluteUri

    let createRequest =
        match proxy with
        | Some proxy ->
            let httpfsProxy: HttpClient.Proxy =
                { Address = proxy.Address
                  Port = int proxy.Port
                  Credentials = match proxy.Credentials with
                                | Some creds -> ProxyCredentials.Custom { username = creds.Username; password = creds.Password }
                                | None -> ProxyCredentials.None }
            fun action url -> createRequest action url |> withProxy httpfsProxy
        | None -> createRequest

    let invCulture = System.Globalization.CultureInfo.InvariantCulture

    let withQueryStringItems items request =
        items |> List.fold (swap withQueryStringItem) request

    let buildError (response: Response) =
        let code = enum response.StatusCode
        let msg = match response.EntityBody with
                  | Some body -> Some (body.Trim())
                  | None -> None
        Fail (HttpError (code, msg))

    let query cmd fn = asyncChoice {
        let! response =
            createRequest Get (url "query")
            |> withQueryStringItem { name="q"; value=cmd }
            |> getResponseAsync
            |> Async.Catch
            <!!> TransportError

        return!
            match enum response.StatusCode with
            | HttpStatusCode.OK -> fn response
            | _ -> buildError response
    }

    let write query body = asyncChoice {
        let! response =
            createRequest Post (url "write")
            |> withQueryStringItems query
            |> withBody body
            |> getResponseAsync
            |> Async.Catch
            <!!> TransportError

        return!
            match enum response.StatusCode with
            | HttpStatusCode.NoContent -> Ok ()
            | _ -> buildError response
    }

    let ok _ = Ok ()

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

    let showDbs () =
        query "SHOW DATABASES" <| fun resp ->
            choice {
                let! json = resp.EntityBody |> Choice.ofOption |> Choice.mapFail (fun _ -> OtherError "Response doesnt contain body")
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

    let createDb name =
        query (sprintf "CREATE DATABASE %s" name) ok

    let dropDb name =
        query (sprintf "DROP DATABASE %s" name) ok

    // todo escaping of tags and fields with tests (see docs)
    // todo sort tags by keys for perfomance (see docs)
    // todo check point has at least one field
    // todo double to string should have at least one 0 after .
    // todo encode booleans as 't' and 'f' for perfomance
    // todo surround string field values with " with escaping
    // todo rewrite with stringBuffer{} and run under profiler
    // todo validate Measurement (not null, not empty string)
    // todo validate Fields should have at least one value
    // todo validate db name
    // docs: https://influxdb.com/docs/v0.9/write_protocols/line.html
    let write db point precision =
        let tags =
            point.Tags
            |> Map.toSeq
            |> Seq.map ((<||) (sprintf "%s=%s"))
            |> String.concat ","
            |> function "" -> "" | s -> sprintf ",%s" s
        let key = sprintf "%s%s" point.Measurement tags

        let fields =
            point.Fields
            |> Map.toSeq
            |> Seq.map (fun (k, v) ->
                let value =
                    match v with
                    | Int32 v -> string v
                    | Int64 v -> string v
                    | Double v -> v.ToString("0.0###############", invCulture)
                    | String v -> sprintf "\"%s\"" v
                sprintf "%s=%s" k value)
            |> String.concat ","

        let timestamp, precision =
            // todo reorder for perfmance?
            match precision with
            | Precision.Nanoseconds -> DateTime.toUnixNanoseconds point.Timestamp, "n"
            | Precision.Microseconds -> DateTime.toUnixMicroseconds point.Timestamp, "u"
            | Precision.Milliseconds -> DateTime.toUnixMilliseconds point.Timestamp, "ms"
            | Precision.Seconds -> DateTime.toUnixSeconds point.Timestamp, "s"
            | Precision.Minutes -> DateTime.toUnixMinutes point.Timestamp, "m"
            | Precision.Hours -> DateTime.toUnixHours point.Timestamp, "h"
            | x -> failwithf "unknown precision %A" x

        let line = sprintf "%s %s %d" key fields timestamp
        let query = [ { name="db"; value=db }
                      { name="precision"; value=precision } ]
        write query line

    member __.Ping() = ping()

    member __.ShowDatabases() = showDbs()
    member __.CreateDatabase(name: string) = createDb name
    member __.DropDatabase(name: string) = dropDb name

    // todo write warning in xml doc about better usage of WriteMany
    member __.Write(db: Database, point: Point, precision: Precision) = write db point precision
