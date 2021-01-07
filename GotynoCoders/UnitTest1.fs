module Program

open Fuchu
open Thoth.Json.Net

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

          test "`decodeTypeTag` & `decodeWithTypeTag` works" {
              let typeTagDecoder = GotynoCoders.decodeTypeTag "kind"

              let passingTypeTagValue =
                  Encode.object [ "kind", Encode.string "TypeTag"
                                  "other", Encode.uint32 42u ]
                  |> Encode.toString 2

              let passingTypeTagResult =
                  Decode.fromString typeTagDecoder passingTypeTagValue

              Assert.Equal("Type tag result with `kind` field passes", passingTypeTagResult, (Ok "TypeTag"))
          } ]

[<EntryPoint>]
let main arguments = defaultMainThisAssembly arguments
