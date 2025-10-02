module OpenApi.Common exposing
    ( encodeStringDateTime, toParamStringStringDateTime
    , decodeStringDateTime
    , Nullable(..), Error(..), jsonDecodeAndMap, decodeOptionalField
    )

{-|


## Encoders

@docs encodeStringDateTime, toParamStringStringDateTime


## Decoders

@docs decodeStringDateTime


## Common

@docs Nullable, Error, jsonDecodeAndMap, decodeOptionalField

-}

import Http
import Json.Decode
import Json.Encode
import Parser.Advanced exposing ((|.), (|=))
import Rfc3339
import Time


encodeStringDateTime : Time.Posix -> Json.Encode.Value
encodeStringDateTime value =
    Json.Encode.string
        (Rfc3339.toString
            (Rfc3339.DateTimeOffset
                { instant = value, offset = { hour = 0, minute = 0 } }
            )
        )


toParamStringStringDateTime : Time.Posix -> String
toParamStringStringDateTime value =
    Rfc3339.toString
        (Rfc3339.DateTimeOffset
            { instant = value, offset = { hour = 0, minute = 0 } }
        )


decodeStringDateTime : Json.Decode.Decoder Time.Posix
decodeStringDateTime =
    Json.Decode.andThen
        (\andThenUnpack ->
            case
                Parser.Advanced.run Rfc3339.dateTimeOffsetParser andThenUnpack
            of
                Result.Ok value ->
                    Json.Decode.succeed value.instant

                Result.Err error ->
                    Json.Decode.fail "Invalid RFC-3339 date-time"
        )
        Json.Decode.string


type Nullable value
    = Null
    | Present value


type Error err body
    = BadUrl String
    | Timeout
    | NetworkError
    | KnownBadStatus Int err
    | UnknownBadStatus Http.Metadata body
    | BadErrorBody Http.Metadata body
    | BadBody Http.Metadata body


{-| Chain JSON decoders, when `Json.Decode.map8` isn't enough.
-}
jsonDecodeAndMap :
    Json.Decode.Decoder a
    -> Json.Decode.Decoder (a -> value)
    -> Json.Decode.Decoder value
jsonDecodeAndMap dx df =
    Json.Decode.map2 (|>) dx df


{-| Decode an optional field

    decodeString (decodeOptionalField "x" int) "{ "x": 3 }"
    --> Ok (Just 3)

    decodeString (decodeOptionalField "x" int) "{ "x": true }"
    --> Err ...

    decodeString (decodeOptionalField "x" int) "{ "y": 4 }"
    --> Ok Nothing

-}
decodeOptionalField : String -> Json.Decode.Decoder t -> Json.Decode.Decoder (Maybe t)
decodeOptionalField key fieldDecoder =
    Json.Decode.andThen
        (\andThenUnpack ->
            if andThenUnpack then
                Json.Decode.field
                    key
                    (Json.Decode.oneOf
                        [ Json.Decode.map Just fieldDecoder
                        , Json.Decode.null Nothing
                        ]
                    )

            else
                Json.Decode.succeed Nothing
        )
        (Json.Decode.oneOf
            [ Json.Decode.map
                (\_ -> True)
                (Json.Decode.field key Json.Decode.value)
            , Json.Decode.succeed False
            ]
        )
