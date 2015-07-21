namespace InfluxDB.FSharp

type Measurement = string

type FieldValue =
    | Int of int64
    | Float of double
    | String of string
    | Bool of bool
    with override x.ToString() =
            match x with
            | Int x -> string x
            | Float x -> string x
            | String x -> x
            | Bool x -> string x

type Precision =
    | Microseconds = 0
    | Milliseconds = 1
    | Seconds = 2
    | Minutes = 3
    | Hours = 4
