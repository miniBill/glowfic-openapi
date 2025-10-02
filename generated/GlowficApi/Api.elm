module GlowficApi.Api exposing
    ( postsId
    , boardSectionsReorder
    )

{-|


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


{-| Load a single post as a JSON resource
-}
postsId :
    { authorization : { glowfic_constellation_production : String }
    , params : { id : Int }
    }
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
        , headers =
            [ ( "Cookie"
              , "_glowfic_constellation_production=" ++ config.authorization.glowfic_constellation_production
              )
            ]
        , body = BackendTask.Http.emptyBody
        , retries = Nothing
        , timeoutInMs = Nothing
        }
        (BackendTask.Http.expectJson GlowficApi.Json.decodePost)


{-| Update the order of subcontinuities. This is an unstable feature, and may be moved or renamed; it should not be trusted.
-}
boardSectionsReorder :
    { authorization : { glowfic_constellation_production : String }
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
        , headers =
            [ ( "Cookie"
              , "_glowfic_constellation_production=" ++ config.authorization.glowfic_constellation_production
              )
            ]
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
