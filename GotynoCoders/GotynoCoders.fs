module GotynoCoders

open Thoth.Json.Net

let encodeList encoder value =
    value
    |> List.map encoder
    |> Encode.list

let decodeLiteral decoder literal: Decoder<'a> =
    let decodeLiteral =
        function
        | l when l = literal -> Decode.succeed literal
        | other -> Decode.fail (sprintf "Expected literal value %A, got: %A" literal other)
        
    decoder |> Decode.andThen decodeLiteral

let decodeLiteralString s: Decoder<string> =
    decodeLiteral Decode.string s

let decodeLiteralUnsignedInteger u: Decoder<uint64> =
    decodeLiteral Decode.uint64 u

let decodeLiteralSignedInteger i: Decoder<int64> =
    decodeLiteral Decode.int64 i
