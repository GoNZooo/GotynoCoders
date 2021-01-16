module Program

open Fuchu
open Thoth.Json.Net

type LogInData =
    {
        username: string
        password: string
    }

    static member Decoder: Decoder<LogInData> =
        Decode.object (fun get ->
            {
              username = get.Required.Field "username" Decode.string
              password = get.Required.Field "password" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "username", Encode.string value.username
                "password", Encode.string value.password
            ]

type UserId =
    {
        value: string
    }

    static member Decoder: Decoder<UserId> =
        Decode.object (fun get ->
            {
              value = get.Required.Field "value" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "value", Encode.string value.value
            ]

type Channel =
    {
        name: string
        ``private``: bool
    }

    static member Decoder: Decoder<Channel> =
        Decode.object (fun get ->
            {
              name = get.Required.Field "name" Decode.string
              ``private`` = get.Required.Field "private" Decode.bool
            }
        )

    static member Encoder value =
        Encode.object
            [
                "name", Encode.string value.name
                "private", Encode.bool value.``private``
            ]

type Email =
    {
        value: string
    }

    static member Decoder: Decoder<Email> =
        Decode.object (fun get ->
            {
              value = get.Required.Field "value" Decode.string
            }
        )

    static member Encoder value =
        Encode.object
            [
                "value", Encode.string value.value
            ]

type Event =
    | LogIn of LogInData
    | LogOut of UserId
    | JoinChannels of list<Channel>
    | SetEmails of list<Email>
    | Close

    static member LogInDecoder: Decoder<Event> =
        Decode.object (fun get -> LogIn(get.Required.Field "data" LogInData.Decoder))

    static member LogOutDecoder: Decoder<Event> =
        Decode.object (fun get -> LogOut(get.Required.Field "data" UserId.Decoder))

    static member JoinChannelsDecoder: Decoder<Event> =
        Decode.object (fun get -> JoinChannels(get.Required.Field "data" (Decode.list Channel.Decoder)))

    static member SetEmailsDecoder: Decoder<Event> =
        Decode.object (fun get -> SetEmails(get.Required.Field "data" (Decode.list Email.Decoder)))

    static member CloseDecoder: Decoder<Event> =
        Decode.object (fun _get -> Close)

    static member Decoder: Decoder<Event> =
        GotynoCoders.decodeWithTypeTag
            "type"
            [|
                "LogIn", Event.LogInDecoder
                "LogOut", Event.LogOutDecoder
                "JoinChannels", Event.JoinChannelsDecoder
                "SetEmails", Event.SetEmailsDecoder
                "Close", Event.CloseDecoder
            |]

    static member Encoder =
        function
        | LogIn payload ->
            Encode.object [ "type", Encode.string "LogIn"
                            "data", LogInData.Encoder payload ]

        | LogOut payload ->
            Encode.object [ "type", Encode.string "LogOut"
                            "data", UserId.Encoder payload ]

        | JoinChannels payload ->
            Encode.object [ "type", Encode.string "JoinChannels"
                            "data", GotynoCoders.encodeList Channel.Encoder payload ]

        | SetEmails payload ->
            Encode.object [ "type", Encode.string "SetEmails"
                            "data", GotynoCoders.encodeList Email.Encoder payload ]

        | Close ->
            Encode.object [ "type", Encode.string "Close" ]

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
    | NoPayload

    static member OneDecoder: Decoder<OneTwoUnion> =
        Decode.object (fun get -> One(get.Required.Field "data" OnePayload.Decoder))

    static member TwoDecoder: Decoder<OneTwoUnion> =
        Decode.object (fun get -> Two(get.Required.Field "data" TwoPayload.Decoder))

    static member NoPayloadDecoder: Decoder<OneTwoUnion> = Decode.succeed NoPayload

    static member Decoder: Decoder<OneTwoUnion> =
        GotynoCoders.decodeWithTypeTag
            "kind"
            [| "One", OneTwoUnion.OneDecoder
               "Two", OneTwoUnion.TwoDecoder
               "NoPayload", OneTwoUnion.NoPayloadDecoder |]

    static member Encoder =
        function
        | One payload ->
            Encode.object [ "kind", Encode.string "One"
                            "data", OnePayload.Encoder payload ]
        | Two payload ->
            Encode.object [ "kind", Encode.string "Two"
                            "data", TwoPayload.Encoder payload ]
        | NoPayload -> Encode.object [ "kind", Encode.string "NoPayload" ]

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

              Assert.Equal
                  ("Union decoding result with payload passes", passingUnionResult, (Ok(One { OneField = 42u })))

              let passingEmptyUnionValue =
                  NoPayload
                  |> OneTwoUnion.Encoder
                  |> Encode.toString 2

              let passingEmptyUnionResult =
                  Decode.fromString unionDecoder passingEmptyUnionValue

              Assert.Equal("Union decoding result without payload case passes", passingEmptyUnionResult, (Ok NoPayload))

              let failingUnionValue =
                  Encode.object [ "kind", Encode.string "InvalidTag"
                                  "data", Encode.object [ "/shrug", Encode.string "value" ] ]
                  |> Encode.toString 2

              let failingUnionResult =
                  Decode.fromString unionDecoder failingUnionValue

              let errorString =
                  "Error at: `$`\nThe following `failure` occurred with the decoder: Found tag does not match any in specification, expecting one of 'One', 'Two', 'NoPayload', got: InvalidTag"

              Assert.Equal("Invalid result is invalid :D", failingUnionResult, (Error errorString))
          } ]

[<EntryPoint>]
let main arguments = defaultMainThisAssembly arguments
