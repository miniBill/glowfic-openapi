module Route.MCU exposing (..)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Http as Http
import Dict exposing (Dict)
import Dict.Extra
import FatalError exposing (FatalError)
import GlowficApi.Api
import GlowficApi.Extra
import GlowficApi.Types exposing (Board, Character, Icon, PostDetails, PostSummary, Reply)
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Html.Attributes
import Id exposing (Id(..))
import List.Extra
import Maybe.Extra
import Pages.Url
import PagesMsg exposing (PagesMsg)
import RouteBuilder exposing (App, StatelessRoute)
import SeqDict exposing (SeqDict)
import SeqSet
import Set
import Url exposing (Url)
import UrlPath
import View exposing (View)
import View.Post


type alias ActionData =
    Never


type alias Data =
    { posts : SeqDict (Id PostDetails) ( PostDetails, List Reply )
    , charactersIcons : SeqDict (Id Character) { id : Id Icon, url : Url }
    }


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    {}


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.single
        { head = head
        , data = data
        }
        |> RouteBuilder.buildNoState { view = view }


head : App Data ActionData RouteParams -> List Head.Tag
head _ =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = [ "images", "icon-png.png" ] |> UrlPath.join |> Pages.Url.fromPath
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "Welcome to elm-pages!"
        , locale = Nothing
        , title = "elm-pages is running"
        }
        |> Seo.website


continuityId : Id Board
continuityId =
    Id 4968


data : BackendTask FatalError Data
data =
    Do.do GlowficApi.Extra.login <| \authorization ->
    Do.do (GlowficApi.Extra.getAllBoardsIdPosts authorization continuityId) <| \results ->
    Do.each results (\{ id } -> GlowficApi.Extra.getPost authorization (Id id)) <| \posts ->
    let
        charactersIds : List (Id Character)
        charactersIds =
            allCharactersIds posts
    in
    Do.each charactersIds (getCharacterIcon authorization) <| \charactersIcons ->
    { charactersIcons =
        charactersIcons
            |> List.filterMap (\( f, s ) -> Maybe.map (Tuple.pair f) s)
            |> SeqDict.fromList
    , posts =
        posts
            |> List.map
                (\( p, r ) ->
                    let
                        id : Id PostDetails
                        id =
                            Id p.id
                    in
                    ( id, ( p, r ) )
                )
            |> SeqDict.fromList
    }
        |> BackendTask.succeed


getCharacterIcon : { token : String } -> Id Character -> BackendTask FatalError ( Id Character, Maybe { id : Id Icon, url : Url } )
getCharacterIcon authorization id =
    GlowficApi.Extra.getCharacterIcon authorization id
        |> BackendTask.map (\icon -> ( id, icon ))


allCharactersIds : List ( PostDetails, List Reply ) -> List (Id Character)
allCharactersIds list =
    list
        |> List.concatMap (\( post, replies ) -> post.character :: List.map .character replies)
        |> Maybe.Extra.values
        |> List.map (\{ id } -> Id id)
        |> SeqSet.fromList
        |> SeqSet.toList


view : App Data ActionData {} -> Model -> View msg
view app model =
    { title = "MCU"
    , body =
        app.data.posts
            |> SeqDict.toList
            |> List.map
                (\( _, ( post, replies ) ) ->
                    Html.div
                        [ Html.Attributes.style "border" "1px solid black"
                        , Html.Attributes.style "padding" "8px"
                        , Html.Attributes.style "display" "flex"
                        , Html.Attributes.style "flex-direction" "column"
                        ]
                        (viewPostSummary post replies)
                )
            |> Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "flex-wrap" "wrap"
                , Html.Attributes.style "gap" "8px"
                , Html.Attributes.style "padding" "8px"
                ]
            |> List.singleton
    }


viewPostSummary : PostDetails -> List Reply -> List (Html msg)
viewPostSummary post replies =
    let
        name =
            post.subject
    in
    [ Html.text name
    , (post.icon
        :: List.map
            (\reply -> reply.icon)
            replies
      )
        |> Maybe.Extra.values
        |> List.Extra.uniqueBy .id
        |> List.map
            (\{ id, url } ->
                Html.a
                    [ Html.Attributes.class "icon"
                    , Html.Attributes.href ("https://glowfic.com/icons/" ++ String.fromInt id)
                    ]
                    [ Html.img
                        [ Html.Attributes.style "width" "auto"
                        , Html.Attributes.style "height" "60px"
                        , Html.Attributes.src (Url.toString url)
                        ]
                        []
                    ]
            )
        |> Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-wrap" "wrap"
            , Html.Attributes.style "gap" "8px"
            ]
    ]
