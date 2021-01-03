module GotynoCoders

open Thoth.Json.Net

let decodeLiteralString literalString: Decoder<string> =
    let decodeLiteralString' =
        function
        | s when s = literalString -> Decode.succeed s
        | anythingElse ->
            Decode.fail (sprintf "Expected literal string with value '%s', got '%s'" literalString anythingElse)

    Decode.string
    |> Decode.andThen decodeLiteralString'
