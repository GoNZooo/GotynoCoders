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
                  "Error at: `$`\nThe following `failure` occurred with the decoder: Expected literal string with value 'passingData', got 'passingData1'"

              Assert.Equal("Passing result should be `Ok` with same value", passingResult, (Ok "passingData"))
              Assert.Equal("Failing result should have error description", failingResult, (Error errorDescription))
          } ]

[<EntryPoint>]
let main arguments = defaultMainThisAssembly arguments
