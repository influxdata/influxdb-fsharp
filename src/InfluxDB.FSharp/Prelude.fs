namespace InfluxDB.FSharp

open System

[<AutoOpen>]
module Prelude =
    let inline Ok a: Choice<_, _> = Choice1Of2 a
    let inline Fail a: Choice<_, _> = Choice2Of2 a

    let (|Ok|Fail|) =
        function
        | Choice1Of2 a -> Ok a
        | Choice2Of2 a -> Fail a

    let konst x _ = x

    let asyncChoice = AsyncChoiceBuilder ()

module String =
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


module Choice =
    let ofOption (value : 'T option) : Choice<'T, unit> =
        match value with
        | Some result -> Ok result
        | None -> Fail ()

    let mapFail (mapping : 'Error1 -> 'Error2) (value : Choice<'T, 'Error1>) =
        match value with
        | Ok result -> Ok result
        | Fail error -> Fail (mapping error)

module Async =
    let map (mapping : 'T -> 'U) (value : Async<'T>) : Async<'U> = async {
        let! x = value
        return mapping x
    }

module AsyncChoice =
    let inline mapError (fn: 'b -> 'c) (value: Async<Choice<'a, 'b>>) : Async<Choice<'a, 'c>> =
        value |> Async.map (Choice.mapFail fn)

    let inline (<!!>) value fn = mapError fn value
