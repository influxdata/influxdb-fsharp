namespace InfluxDB.FSharp

open System
open System.Net
open HttpClient
open InfluxDB.FSharp.Choice
open InfluxDB.FSharp.AsyncChoice

module internal Contracts =
    open System.Collections.Generic
    open System.Runtime.Serialization
    open System.Runtime.Serialization.Json

    [<DataContract>]
    type Series =
        {
            [<field: DataMember(Name="name")>]
            Name: string

            [<field: DataMember(Name="tags")>]
            Tags: Dictionary<string,string>

            [<field: DataMember(Name="columns")>]
            Columns: string[]

            [<field: DataMember(Name="values")>]
            Values: obj[][]
        }

    [<DataContract>]
    type Results =
        {
            [<field: DataMember(Name="series")>]
            Series: Series[]

            [<field: DataMember(Name="error")>]
            Error: string
        }

    [<DataContract>]
    type Response =
        {
            [<field: DataMember(Name="results")>]
            Results: Results[]

            [<field: DataMember(Name="error")>]
            Error: string
        }

    let private settings = DataContractJsonSerializerSettings()
    settings.UseSimpleDictionaryFormat <- true

    let deserialize<'a> (json: string) =
        Choice.attempt <| fun () ->
            use ms = new IO.MemoryStream(Text.Encoding.Unicode.GetBytes json)
            let serializer = DataContractJsonSerializer(typedefof<'a>, settings)
            serializer.ReadObject(ms) :?> 'a


// todo xml docs on public members
// todo validate host
// todo validate proxy
type Client (host: string, ?port: uint16, ?credentials: Credentials, ?proxy: InfluxDB.FSharp.Proxy) =
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

    let withQueryStringItems items request =
        items |> List.fold (swap withQueryStringItem) request

    let buildError (response: Response) =
        let code = enum response.StatusCode
        let msg = match response.EntityBody with
                  | Some body ->
                      match Contracts.deserialize<Contracts.Response> body with
                      | Ok resp -> Some resp.Error
                      | Fail _ -> Some body
                  | None -> None
        Fail (HttpError (code, msg))

    let query db qstr mapOk = asyncChoice {
        let withDb =
            match db with
            | Some db -> withQueryStringItem { name="db"; value=db }
            | None -> id

        let! response =
            createRequest Get (url "query")
            |> withDb
            |> withQueryStringItem { name="q"; value=qstr }
            |> getResponseAsync
            |> Async.Catch
            <!!> TransportError

        return!
            match enum response.StatusCode with
            | HttpStatusCode.OK -> mapOk response
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
        query None "SHOW DATABASES" <| fun resp ->
            choice {
                let! json = resp.EntityBody |> Choice.ofOption |> Choice.mapFail (fun _ -> OtherError "Response doesnt contain body")
                let! response = Contracts.deserialize<Contracts.Response> json <!~> ResponseParseError
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

    // todo: refact "<!~> ResponseParseError" somehow
    let checkForError resp =
        choice {
            let! json = resp.EntityBody |> Choice.ofOption <!~> ResponseParseError
            let! resp = Contracts.deserialize<Contracts.Response> json <!~> ResponseParseError
            let! result = resp.Results |> Seq.trySingleC <!~> ResponseParseError
            return!
                match result.Error with
                | null -> Ok ()
                | errmsg -> Fail (Error.ServerError errmsg)
        }

    let createDb name =
        query None (sprintf "CREATE DATABASE %s" name) checkForError

    let dropDb name =
        query None (sprintf "DROP DATABASE %s" name) checkForError


    // todo validate db name
    // todo sort tags by keys for perfomance (see docs)
    // todo rewrite with stringBuffer{} and run under profiler
    let doWrite db (point: Point.T) precision =
        let line = Point.toLine point precision
        let precision =
            // todo reorder for perfmance?
            match precision with
            | Precision.Microseconds -> "u"
            | Precision.Milliseconds -> "ms"
            | Precision.Seconds -> "s"
            | Precision.Minutes -> "m"
            | Precision.Hours -> "h"
            | x -> raise (NotImplementedException(sprintf "precision %A" x))

        let query = [ { name="db"; value=db }
                      { name="precision"; value=precision } ]
        write query line

    let doQuery db querystr =
        query (Some db) querystr <| fun (resp: Response) ->
            choice {
                let! body = Choice.ofOption resp.EntityBody <!~> ResponseParseError
                let! qresp = Contracts.deserialize<Contracts.Response> body <!~> ResponseParseError
                let response =
                    match Option.ofNull qresp.Error with
                    | Some errormsg -> Fail (ServerError errormsg)
                    | None ->
                        qresp.Results
                        |> Array.map (fun res ->
                            match Option.ofNull res.Error with
                            | Some errormsg -> Fail errormsg
                            | None ->
                                res.Series
                                |> Array.emptyIfNull
                                |> Array.map (fun ser ->
                                    { Name = ser.Name
                                      Tags = match Option.ofNull ser.Tags with
                                             | Some tags -> Map.ofDict tags
                                             | None -> Map.empty
                                      Columns = ser.Columns |> Array.toList
                                      Values = ser.Values |> Array.map (Array.map (function
                                                                                   | :? int32 as v -> FieldValue.Int (int64 v)
                                                                                   | :? int64 as v -> FieldValue.Int v
                                                                                   | :? float as v -> FieldValue.Float v
                                                                                   | :? string as v -> FieldValue.String v
                                                                                   | :? bool as v -> FieldValue.Bool v
                                                                                   | x -> failwithf "mappint for %O (%s) not implemented" x (x.GetType().FullName)))
                                     })
                                |> Array.toList
                                |> Ok)
                        |> Array.toList
                        |> Ok
                return! response
            }

    member __.Ping() = ping()

    member __.ShowDatabases() = showDbs()
    member __.CreateDatabase(name: string) = createDb name
    member __.DropDatabase(name: string) = dropDb name

    // todo write warning in xml doc about better usage of WriteMany
    member __.Write(db: Database, point: Point.T, precision: Precision) = doWrite db point precision

    member __.Query(db: Database, query: string) : Async<Choice<QueryResult list,Error>> = doQuery db query
