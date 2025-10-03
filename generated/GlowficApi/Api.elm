module GlowficApi.Api exposing
    ( login
    , postsId
    , boardSectionsReorder
    )

{-|


## Operations

@docs login


## Posts

@docs postsId


## Subcontinuities

@docs boardSectionsReorder

-}

import BackendTask
import BackendTask.Http
import FatalError
import GlowficApi.Json
import GlowficApi.Types
import Json.Decode
import Json.Encode
import OpenApi.Common
import Url.Builder


{-| Login
-}
login :
    { body : { password : String, username : String } }
    ->
        BackendTask.BackendTask
            { fatal : FatalError.FatalError
            , recoverable : BackendTask.Http.Error
            }
            { token : String }
login config =
    BackendTask.Http.request
        { url =
            Url.Builder.crossOrigin "https://glowfic.com/api/v1" [ "login" ] []
        , method = "POST"
        , headers = []
        , body =
            BackendTask.Http.jsonBody
                (Json.Encode.object
                    [ ( "password", Json.Encode.string config.body.password )
                    , ( "username", Json.Encode.string config.body.username )
                    ]
                )
        , retries = Nothing
        , timeoutInMs = Nothing
        }
        (BackendTask.Http.expectJson
            (Json.Decode.succeed
                (\token -> { token = token })
                |> OpenApi.Common.jsonDecodeAndMap
                    (Json.Decode.field "token" Json.Decode.string)
            )
        )


{-| Load a single post as a JSON resource
-}
postsId :
    { authorization : { authorization : String }, params : { id : Int } }
    ->
        BackendTask.BackendTask
            { fatal : FatalError.FatalError
            , recoverable : BackendTask.Http.Error
            }
            GlowficApi.Types.Post
postsId config =
    BackendTask.Http.request
        { url =
            Url.Builder.crossOrigin
                "https://glowfic.com/api/v1"
                [ "posts", String.fromInt config.params.id ]
                []
        , method = "GET"
        , headers = [ ( "Authorization", config.authorization.authorization ) ]
        , body = BackendTask.Http.emptyBody
        , retries = Nothing
        , timeoutInMs = Nothing
        }
        (BackendTask.Http.expectJson GlowficApi.Json.decodePost)


{-| Update the order of subcontinuities. This is an unstable feature, and may be moved or renamed; it should not be trusted.
-}
boardSectionsReorder :
    { authorization : { authorization : String }
    , body :
        { ordered_section_ids : Maybe (List GlowficApi.Types.Int_Or_String) }
    }
    ->
        BackendTask.BackendTask
            { fatal : FatalError.FatalError
            , recoverable : BackendTask.Http.Error
            }
            { section_ids : Maybe (List Int) }
boardSectionsReorder config =
    BackendTask.Http.request
        { url =
            Url.Builder.crossOrigin
                "https://glowfic.com/api/v1"
                [ "board_sections", "reorder" ]
                []
        , method = "POST"
        , headers = [ ( "Authorization", config.authorization.authorization ) ]
        , body =
            BackendTask.Http.jsonBody
                (Json.Encode.object
                    (List.filterMap
                        Basics.identity
                        [ Maybe.map
                            (\mapUnpack ->
                                ( "ordered_section_ids"
                                , Json.Encode.list
                                    (\rec ->
                                        case rec of
                                            GlowficApi.Types.Int_Or_String__Int content ->
                                                Json.Encode.int content

                                            GlowficApi.Types.Int_Or_String__String content ->
                                                Json.Encode.string content
                                    )
                                    mapUnpack
                                )
                            )
                            config.body.ordered_section_ids
                        ]
                    )
                )
        , retries = Nothing
        , timeoutInMs = Nothing
        }
        (BackendTask.Http.expectJson
            (Json.Decode.succeed
                (\section_ids -> { section_ids = section_ids })
                |> OpenApi.Common.jsonDecodeAndMap
                    (OpenApi.Common.decodeOptionalField
                        "section_ids"
                        (Json.Decode.list Json.Decode.int)
                    )
            )
        )
