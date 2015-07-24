namespace InfluxDB.FSharp

open System

module Point =

    type T

    val create : Measurement -> Map<string,string> -> Map<string,FieldValue> -> DateTime option -> Choice<T, string list>
    val toLine : T -> Precision -> string
