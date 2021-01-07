module Program

open Fuchu
open Thoth.Json.Net

type OnePayload =
    { OneField: uint32 }

    static member Decoder: Decoder<OnePayload> =
        Decode.object (fun get -> { OneField = get.Required.Field "OneField" Decode.uint32 })

    static member Encoder value =
        Encode.object [ "OneField", Encode.uint32 value.OneField ]

type TwoPayload =
    { TwoField: string }

    static member Decoder: Decoder<TwoPayload> =
        Decode.object (fun get -> { TwoField = get.Required.Field "OneField" Decode.string })

    static member Encoder value =
        Encode.object [ "TwoField", Encode.string value.TwoField ]

type OneTwoUnion =
    | One of OnePayload
    | Two of TwoPayload

    static member OneDecoder: Decoder<OneTwoUnion> =
        Decode.object (fun get -> One(get.Required.Field "data" OnePayload.Decoder))

    static member TwoDecoder: Decoder<OneTwoUnion> =
        Decode.object (fun get -> Two(get.Required.Field "data" TwoPayload.Decoder))

    static member Decoder: Decoder<OneTwoUnion> =
        GotynoCoders.decodeWithTypeTag
            "kind"
            [| "One", OneTwoUnion.OneDecoder
               "Two", OneTwoUnion.TwoDecoder |]

    static member Encoder =
        function
        | One payload ->
            Encode.object [ "kind", Encode.string "One"
                            "data", OnePayload.Encoder payload ]
        | Two payload ->
            Encode.object [ "kind", Encode.string "Two"
                            "data", TwoPayload.Encoder payload ]

[<Tests>]
let tests =
    testList
        "Basic"
        [ test "`decodeLiteralString` works" {
              let passingValue = "\"passingData\""
              let failingValue = "\"passingData1\""

              let decoder =
                  GotynoCoders.decodeLiteralString "passingData"

              let passingResult = Decode.fromString decoder passingValue
              let failingResult = Decode.fromString decoder failingValue

              let errorDescription =
                  "Error at: `$`\nThe following `failure` occurred with the decoder: Expected literal value \"passingData\", got: \"passingData1\""

              Assert.Equal("Passing result should be `Ok` with same value", passingResult, (Ok "passingData"))
              Assert.Equal("Failing result should have error description", failingResult, (Error errorDescription))
          }

          test "`decodeTypeTag`" {
              let typeTagDecoder = GotynoCoders.decodeTypeTag "kind"

              let passingTypeTagValue =
                  Encode.object [ "kind", Encode.string "TypeTag"
                                  "other", Encode.uint32 42u ]
                  |> Encode.toString 2

              let passingTypeTagResult =
                  Decode.fromString typeTagDecoder passingTypeTagValue

              Assert.Equal("Type tag result with `kind` field passes", passingTypeTagResult, (Ok "TypeTag"))
          }

          test "`decodeWithTypeTag` works" {
              let unionDecoder = OneTwoUnion.Decoder

              let passingUnionValue =
                  One { OneField = 42u }
                  |> OneTwoUnion.Encoder
                  |> Encode.toString 2

              let passingUnionResult =
                  Decode.fromString unionDecoder passingUnionValue

              let failingUnionValue =
                  Encode.object [ "kind", Encode.string "InvalidTag"
                                  "data", Encode.object [ "/shrug", Encode.string "value" ] ]
                  |> Encode.toString 2

              let failingUnionResult =
                  Decode.fromString unionDecoder failingUnionValue

              let errorString =
                  "Error at: `$`\nThe following `failure` occurred with the decoder: Found tag does not match any in specification, expecting one of 'One', 'Two', got: InvalidTag"

              Assert.Equal("Type tag result with `kind` field passes", passingUnionResult, (Ok(One { OneField = 42u })))
              Assert.Equal("Invalid result is invalid :D", failingUnionResult, (Error errorString))
          } ]

[<EntryPoint>]
let main arguments = defaultMainThisAssembly arguments
