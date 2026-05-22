module View.Post exposing (viewPost, viewReply)

import GlowficApi.Types exposing (Character, Icon, PostDetails, Reply, User)
import GlowficRoute
import Html exposing (Html)
import Html.Attributes
import Html.Parser
import Html.Parser.Util
import Id
import Url


viewPost : PostDetails -> Html msg
viewPost post =
    Html.div
        [ Html.Attributes.class "reply" ]
        [ viewCharacter
            { character = post.character
            , icon = post.icon
            , user =
                post.authors
                    |> List.head
                    |> Maybe.withDefault
                        { id = -1
                        , username = ""
                        }
            }
        , Html.div
            [ Html.Attributes.class "content" ]
            (viewPermalink (GlowficRoute.post (Id.for post))
                :: viewContent post
            )
        ]


viewReply : Reply -> Html msg
viewReply reply =
    Html.div
        [ Html.Attributes.class "reply" ]
        [ viewCharacter reply
        , Html.div
            [ Html.Attributes.class "content" ]
            (viewPermalink (GlowficRoute.reply (Id.for reply))
                :: viewContent reply
            )
        ]


viewPermalink : String -> Html msg
viewPermalink url =
    Html.a
        [ Html.Attributes.href url
        , Html.Attributes.class "permalink"
        ]
        [ Html.img
            [ Html.Attributes.src "https://dhtmoj33sf3e0.cloudfront.net/assets/icons/link-bb9df2e290558f33c20c21f4a2a85841eb4ccb1bd09f6266d3e80679f30ccf62.png" ]
            []
        ]


viewCharacter :
    { a
        | icon : Maybe Icon
        , character : Maybe Character
        , user : User
    }
    -> Html msg
viewCharacter reply =
    Html.div
        [ Html.Attributes.class "character" ]
        [ reply.icon |> Maybe.map viewIcon |> Maybe.withDefault (Html.text "")
        , viewNames reply
        ]


viewIcon : Icon -> Html msg
viewIcon { id, url } =
    Html.a
        [ Html.Attributes.class "icon"
        , Html.Attributes.href ("https://glowfic.com/icons/" ++ String.fromInt id)
        ]
        [ Html.img [ Html.Attributes.src (Url.toString url) ] []
        ]


viewNames : { r | character : Maybe Character, user : User } -> Html msg
viewNames reply =
    Html.div
        [ Html.Attributes.class "names"
        ]
        [ viewCharacterNames reply
        , Html.a
            [ Html.Attributes.href ("https://glowfic.com/users/" ++ String.fromInt reply.user.id)
            , Html.Attributes.class "username"
            ]
            [ Html.p [] [ Html.text reply.user.username ]
            ]
        ]


viewCharacterNames : { r | character : Maybe Character } -> Html msg
viewCharacterNames reply =
    case reply.character of
        Nothing ->
            Html.text ""

        Just character ->
            Html.div
                [ Html.Attributes.class "character-name" ]
                [ Html.a
                    [ Html.Attributes.href ("https://glowfic.com/characters/" ++ String.fromInt character.id)
                    ]
                    [ Html.p [] [ Html.text character.name ] ]
                , case character.screenname of
                    Nothing ->
                        Html.text ""

                    Just screenname ->
                        Html.p
                            [ Html.Attributes.class "screenname" ]
                            [ Html.text screenname ]
                ]


viewContent : { a | content : String } -> List (Html msg)
viewContent reply =
    case Html.Parser.run Html.Parser.allCharRefs reply.content of
        Err _ ->
            [ Html.text reply.content ]

        Ok node ->
            Html.Parser.Util.toVirtualDom node
