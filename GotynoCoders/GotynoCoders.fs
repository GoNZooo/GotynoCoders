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

/// Decodes a tag field inside an object and looks up which decoder to apply to the structure, given an associative
/// list of type tags and decoders. This can be useful for decoding unions.
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

type EnumerationPair<'a, 'b> = 'a * 'b
    
type EnumerationSpecification<'a, 'b> = EnumerationPair<'a, 'b> array
    
/// Decodes a value into a base type and compares it to a list of values that all correspond to respective concrete
/// values, then decodes into that concrete value.
let decodeOneOf (valueDecoder: Decoder<'a>) (specification: EnumerationSpecification<'a, 'b>): Decoder<'b> =
    let useMatchingConstructor foundValue =
        match Array.tryFind (fun (t, _) -> t = foundValue) specification with
        | Some (_, constructor) -> Decode.succeed constructor
        | None ->
            let specificationTags =
                specification
                |> Array.fold (fun array (t, _) -> Array.append array [| (sprintf "'%s'" t) |]) Array.empty
                |> String.concat ", "
                
            Decode.fail
                (sprintf
                    "Found tag does not match any in specification, expecting one of %s, got: %s"
                     specificationTags
                     foundValue)
    valueDecoder
    |> Decode.andThen useMatchingConstructor