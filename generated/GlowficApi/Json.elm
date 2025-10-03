module GlowficApi.Json exposing
    ( encodeActive, encodePost, encodeReply
    , decodeActive, decodePost, decodeReply
    )

{-|


## Encoders

@docs encodeActive, encodePost, encodeReply


## Decoders

@docs decodeActive, decodePost, decodeReply

-}

import GlowficApi.Types
import Json.Decode
import Json.Encode
import OpenApi.Common
import Time


encodeActive : GlowficApi.Types.Active -> Json.Encode.Value
encodeActive rec =
    Json.Encode.string (GlowficApi.Types.activeToString rec)


encodePost : GlowficApi.Types.Post -> Json.Encode.Value
encodePost rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Maybe.map
                (\mapUnpack ->
                    ( "authors"
                    , Json.Encode.list
                        (\rec0 ->
                            Json.Encode.object
                                (List.filterMap
                                    Basics.identity
                                    [ Maybe.map
                                        (\mapUnpack0 ->
                                            ( "id", Json.Encode.int mapUnpack0 )
                                        )
                                        rec0.id
                                    , Maybe.map
                                        (\mapUnpack0 ->
                                            ( "username"
                                            , Json.Encode.string mapUnpack0
                                            )
                                        )
                                        rec0.username
                                    ]
                                )
                        )
                        mapUnpack
                    )
                )
                rec.authors
            , Maybe.map
                (\mapUnpack ->
                    ( "board"
                    , Json.Encode.object
                        (List.filterMap
                            Basics.identity
                            [ Just ( "id", Json.Encode.int mapUnpack.id )
                            , Maybe.map
                                (\mapUnpack0 ->
                                    ( "name", Json.Encode.string mapUnpack0 )
                                )
                                mapUnpack.name
                            ]
                        )
                    )
                )
                rec.board
            , Maybe.map
                (\mapUnpack ->
                    ( "character"
                    , Json.Encode.object
                        [ ( "id", Json.Encode.int mapUnpack.id )
                        , ( "name", Json.Encode.string mapUnpack.name )
                        , ( "screenname"
                          , case mapUnpack.screenname of
                                OpenApi.Common.Null ->
                                    Json.Encode.null

                                OpenApi.Common.Present value ->
                                    Json.Encode.string value
                          )
                        ]
                    )
                )
                rec.character
            , Just ( "content", Json.Encode.string rec.content )
            , Maybe.map
                (\mapUnpack ->
                    ( "created_at"
                    , OpenApi.Common.encodeStringDateTime mapUnpack
                    )
                )
                rec.created_at
            , Maybe.map
                (\mapUnpack -> ( "description", Json.Encode.string mapUnpack ))
                rec.description
            , Maybe.map
                (\mapUnpack ->
                    ( "icon"
                    , Json.Encode.object
                        (List.filterMap
                            Basics.identity
                            [ Maybe.map
                                (\mapUnpack0 ->
                                    ( "id", Json.Encode.int mapUnpack0 )
                                )
                                mapUnpack.id
                            , Maybe.map
                                (\mapUnpack0 ->
                                    ( "keyword"
                                    , Json.Encode.string mapUnpack0
                                    )
                                )
                                mapUnpack.keyword
                            , Maybe.map
                                (\mapUnpack0 ->
                                    ( "url", Json.Encode.string mapUnpack0 )
                                )
                                mapUnpack.url
                            ]
                        )
                    )
                )
                rec.icon
            , Just ( "id", Json.Encode.int rec.id )
            , Maybe.map
                (\mapUnpack -> ( "num_replies", Json.Encode.int mapUnpack ))
                rec.num_replies
            , Maybe.map
                (\mapUnpack ->
                    ( "section"
                    , case mapUnpack of
                        OpenApi.Common.Null ->
                            Json.Encode.null

                        OpenApi.Common.Present value ->
                            Json.Encode.object []
                    )
                )
                rec.section
            , Maybe.map
                (\mapUnpack -> ( "section_order", Json.Encode.int mapUnpack ))
                rec.section_order
            , Maybe.map
                (\mapUnpack -> ( "status", encodeActive mapUnpack ))
                rec.status
            , Maybe.map
                (\mapUnpack -> ( "subject", Json.Encode.string mapUnpack ))
                rec.subject
            , Maybe.map
                (\mapUnpack ->
                    ( "tagged_at"
                    , OpenApi.Common.encodeStringDateTime mapUnpack
                    )
                )
                rec.tagged_at
            ]
        )


encodeReply : GlowficApi.Types.Reply -> Json.Encode.Value
encodeReply rec =
    Json.Encode.object
        (List.filterMap
            Basics.identity
            [ Maybe.map
                (\mapUnpack ->
                    ( "character"
                    , Json.Encode.object
                        (List.filterMap
                            Basics.identity
                            [ Maybe.map
                                (\mapUnpack0 ->
                                    ( "id", Json.Encode.int mapUnpack0 )
                                )
                                mapUnpack.id
                            , Maybe.map
                                (\mapUnpack0 ->
                                    ( "name", Json.Encode.string mapUnpack0 )
                                )
                                mapUnpack.name
                            , Maybe.map
                                (\mapUnpack0 ->
                                    ( "screenname"
                                    , Json.Encode.string mapUnpack0
                                    )
                                )
                                mapUnpack.screenname
                            ]
                        )
                    )
                )
                rec.character
            , Maybe.map
                (\mapUnpack ->
                    ( "character_name", Json.Encode.string mapUnpack )
                )
                rec.character_name
            , Maybe.map
                (\mapUnpack -> ( "content", Json.Encode.string mapUnpack ))
                rec.content
            , Maybe.map
                (\mapUnpack ->
                    ( "created_at"
                    , OpenApi.Common.encodeStringDateTime mapUnpack
                    )
                )
                rec.created_at
            , Maybe.map
                (\mapUnpack ->
                    ( "icon"
                    , Json.Encode.object
                        (List.filterMap
                            Basics.identity
                            [ Maybe.map
                                (\mapUnpack0 ->
                                    ( "id", Json.Encode.int mapUnpack0 )
                                )
                                mapUnpack.id
                            , Maybe.map
                                (\mapUnpack0 ->
                                    ( "keyword"
                                    , Json.Encode.string mapUnpack0
                                    )
                                )
                                mapUnpack.keyword
                            , Maybe.map
                                (\mapUnpack0 ->
                                    ( "url", Json.Encode.string mapUnpack0 )
                                )
                                mapUnpack.url
                            ]
                        )
                    )
                )
                rec.icon
            , Maybe.map
                (\mapUnpack -> ( "id", Json.Encode.int mapUnpack ))
                rec.id
            , Maybe.map
                (\mapUnpack ->
                    ( "updated_at"
                    , OpenApi.Common.encodeStringDateTime mapUnpack
                    )
                )
                rec.updated_at
            , Maybe.map
                (\mapUnpack ->
                    ( "user"
                    , Json.Encode.object
                        (List.filterMap
                            Basics.identity
                            [ Maybe.map
                                (\mapUnpack0 ->
                                    ( "id", Json.Encode.int mapUnpack0 )
                                )
                                mapUnpack.id
                            , Maybe.map
                                (\mapUnpack0 ->
                                    ( "username"
                                    , Json.Encode.string mapUnpack0
                                    )
                                )
                                mapUnpack.username
                            ]
                        )
                    )
                )
                rec.user
            ]
        )


decodeActive : Json.Decode.Decoder GlowficApi.Types.Active
decodeActive =
    Json.Decode.andThen
        (\andThenUnpack ->
            case GlowficApi.Types.activeFromString andThenUnpack of
                Maybe.Just a ->
                    Json.Decode.succeed a

                Maybe.Nothing ->
                    Json.Decode.fail
                        (andThenUnpack ++ " is not a valid Active")
        )
        Json.Decode.string


decodePost : Json.Decode.Decoder GlowficApi.Types.Post
decodePost =
    Json.Decode.succeed
        (\authors board character content created_at description icon id num_replies section section_order status subject tagged_at ->
            { authors = authors
            , board = board
            , character = character
            , content = content
            , created_at = created_at
            , description = description
            , icon = icon
            , id = id
            , num_replies = num_replies
            , section = section
            , section_order = section_order
            , status = status
            , subject = subject
            , tagged_at = tagged_at
            }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "authors"
                (Json.Decode.list
                    (Json.Decode.succeed
                        (\id username ->
                            { id = id
                            , username = username
                            }
                        )
                        |> OpenApi.Common.jsonDecodeAndMap
                            (OpenApi.Common.decodeOptionalField
                                "id"
                                Json.Decode.int
                            )
                        |> OpenApi.Common.jsonDecodeAndMap
                            (OpenApi.Common.decodeOptionalField
                                "username"
                                Json.Decode.string
                            )
                    )
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "board"
                (Json.Decode.succeed
                    (\id name ->
                        { id = id
                        , name = name
                        }
                    )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (Json.Decode.field
                            "id"
                            Json.Decode.int
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (OpenApi.Common.decodeOptionalField
                            "name"
                            Json.Decode.string
                        )
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "character"
                (Json.Decode.succeed
                    (\id name screenname ->
                        { id =
                            id
                        , name =
                            name
                        , screenname =
                            screenname
                        }
                    )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (Json.Decode.field
                            "id"
                            Json.Decode.int
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (Json.Decode.field
                            "name"
                            Json.Decode.string
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (Json.Decode.field
                            "screenname"
                            (Json.Decode.oneOf
                                [ Json.Decode.map
                                    OpenApi.Common.Present
                                    Json.Decode.string
                                , Json.Decode.null
                                    OpenApi.Common.Null
                                ]
                            )
                        )
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "content"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "created_at"
                OpenApi.Common.decodeStringDateTime
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "description"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "icon"
                (Json.Decode.succeed
                    (\id keyword url ->
                        { id =
                            id
                        , keyword =
                            keyword
                        , url =
                            url
                        }
                    )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (OpenApi.Common.decodeOptionalField
                            "id"
                            Json.Decode.int
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (OpenApi.Common.decodeOptionalField
                            "keyword"
                            Json.Decode.string
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (OpenApi.Common.decodeOptionalField
                            "url"
                            Json.Decode.string
                        )
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (Json.Decode.field
                "id"
                Json.Decode.int
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "num_replies"
                Json.Decode.int
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "section"
                (Json.Decode.oneOf
                    [ Json.Decode.map
                        OpenApi.Common.Present
                        (Json.Decode.succeed
                            {}
                        )
                    , Json.Decode.null
                        OpenApi.Common.Null
                    ]
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "section_order"
                Json.Decode.int
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "status"
                decodeActive
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "subject"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "tagged_at"
                OpenApi.Common.decodeStringDateTime
            )


decodeReply : Json.Decode.Decoder GlowficApi.Types.Reply
decodeReply =
    Json.Decode.succeed
        (\character character_name content created_at icon id updated_at user ->
            { character = character
            , character_name = character_name
            , content = content
            , created_at = created_at
            , icon = icon
            , id = id
            , updated_at = updated_at
            , user = user
            }
        )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "character"
                (Json.Decode.succeed
                    (\id name screenname ->
                        { id = id
                        , name = name
                        , screenname = screenname
                        }
                    )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (OpenApi.Common.decodeOptionalField
                            "id"
                            Json.Decode.int
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (OpenApi.Common.decodeOptionalField
                            "name"
                            Json.Decode.string
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (OpenApi.Common.decodeOptionalField
                            "screenname"
                            Json.Decode.string
                        )
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "character_name"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "content"
                Json.Decode.string
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "created_at"
                OpenApi.Common.decodeStringDateTime
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "icon"
                (Json.Decode.succeed
                    (\id keyword url ->
                        { id =
                            id
                        , keyword =
                            keyword
                        , url =
                            url
                        }
                    )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (OpenApi.Common.decodeOptionalField
                            "id"
                            Json.Decode.int
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (OpenApi.Common.decodeOptionalField
                            "keyword"
                            Json.Decode.string
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (OpenApi.Common.decodeOptionalField
                            "url"
                            Json.Decode.string
                        )
                )
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "id"
                Json.Decode.int
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "updated_at"
                OpenApi.Common.decodeStringDateTime
            )
        |> OpenApi.Common.jsonDecodeAndMap
            (OpenApi.Common.decodeOptionalField
                "user"
                (Json.Decode.succeed
                    (\id username ->
                        { id =
                            id
                        , username =
                            username
                        }
                    )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (OpenApi.Common.decodeOptionalField
                            "id"
                            Json.Decode.int
                        )
                    |> OpenApi.Common.jsonDecodeAndMap
                        (OpenApi.Common.decodeOptionalField
                            "username"
                            Json.Decode.string
                        )
                )
            )
