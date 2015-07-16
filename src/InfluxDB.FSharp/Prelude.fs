namespace InfluxDB.FSharp

open System

[<AutoOpen>]
module internal Prelude =
    let inline Ok a: Choice<_, _> = Choice1Of2 a
    let inline Fail a: Choice<_, _> = Choice2Of2 a

    let (|Ok|Fail|) =
        function
        | Choice1Of2 a -> Ok a
        | Choice2Of2 a -> Fail a

    let konst value ignored = value

    let choice = ChoiceBuilder ()
    let asyncChoice = AsyncChoiceBuilder ()

module internal String =
    let excludePrefix prefix str =
        if String.IsNullOrEmpty str then str
        elif str.StartsWith prefix then str.[prefix.Length..]
        else str

    let excludeSuffix suffix str =
        if String.IsNullOrEmpty str then str
        elif str.EndsWith suffix then str.[..str.Length - suffix.Length - 1]
        else str

    let ensureEndsWith suffix (str: string) =
        if str.EndsWith suffix then str
        else str + suffix

module internal Seq =
    let trySingle (source: _ seq) =
        use e = source.GetEnumerator()
        if e.MoveNext() then
            let first = e.Current
            if e.MoveNext() = false then Some first
            else None
        else None

module internal Option =
    let inline ofNull value =
        if Object.ReferenceEquals(value, null) then None else Some value

module internal Choice =
    let ofOption (value : 'T option) : Choice<'T, unit> =
        match value with
        | Some result -> Ok result
        | None -> Fail ()

    let mapFail (mapping : 'err1 -> 'err2) (value : Choice<'t, 'err1>) =
        match value with
        | Ok result -> Ok result
        | Fail error -> Fail (mapping error)

    let attempt fn =
        try
            Ok (fn())
        with
        | exn -> Fail exn

    let inline (<!>) value fn = mapFail fn value
    let inline (<!~>) value error = mapFail (konst error) value

module internal Async =
    let map (mapping : 'T -> 'U) (value : Async<'T>) : Async<'U> = async {
        let! x = value
        return mapping x
    }

module internal AsyncChoice =
    let inline mapError (fn: 'b -> 'c) (value: Async<Choice<'a, 'b>>) : Async<Choice<'a, 'c>> =
        value |> Async.map (Choice.mapFail fn)

    let inline (<!!>) value fn = mapError fn value
