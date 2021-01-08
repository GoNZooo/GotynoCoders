module GotynoCoders

open Thoth.Json.Net

let encodeList encoder value = value |> List.map encoder |> Encode.list

let decodeLiteral decoder literal: Decoder<'a> =
    let decodeLiteral =
        function
        | l when l = literal -> Decode.succeed literal
        | other -> Decode.fail (sprintf "Expected literal value %A, got: %A" literal other)

    decoder |> Decode.andThen decodeLiteral

let decodeLiteralString s: Decoder<string> = decodeLiteral Decode.string s

let decodeLiteralUnsignedInteger u: Decoder<uint64> = decodeLiteral Decode.uint64 u

let decodeLiteralSignedInteger i: Decoder<int64> = decodeLiteral Decode.int64 i

/// Decodes a field as a string, meant to contain a type tag
let decodeTypeTag tagField =
    Decode.object (fun get -> get.Required.Field tagField Decode.string)

type DecoderPair<'a> = string * Decoder<'a>

type DecoderSpecification<'a> = DecoderPair<'a> array

let decodeWithTypeTag tagField (specification: DecoderSpecification<'a>): Decoder<'a> =
    let applyMatchingDecoder foundTag =
        match Array.tryFind (fun (t, _) -> t = foundTag) specification with
        | Some (_, decoder) -> decoder
        | None ->
            let specificationTags =
                specification
                |> Array.fold (fun array (t, _) -> Array.append array [| (sprintf "'%s'" t) |]) Array.empty
                |> String.concat ", "

            Decode.fail
                (sprintf
                    "Found tag does not match any in specification, expecting one of %s, got: %s"
                     specificationTags
                     foundTag)

    decodeTypeTag tagField
    |> Decode.andThen applyMatchingDecoder
