module Route.Timeline.Post.Id_ exposing (ActionData, Data, MessageId, Model, Msg, RouteParams, route)

import BackendTask exposing (BackendTask)
import Color.Oklch exposing (Oklch)
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import GlowficApi.Extra
import GlowficApi.Types exposing (Character, Icon, PostDetails, Reply)
import Head
import Head.Seo as Seo
import Http
import Id exposing (Id, PostId)
import Monad exposing (Monad)
import Monad.Do as Do
import Pages.Url
import PagesMsg exposing (PagesMsg)
import Parser
import RouteBuilder exposing (App, StatelessRoute)
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)
import Server.Response as Response exposing (Response)
import Url exposing (Url)
import UrlPath
import View exposing (View)
import View.Post


type alias ActionData =
    Never


type alias Data =
    { name : String
    , post : ( PostDetails, List Reply )
    , enter : SeqDict (Id Reply) (SeqSet (Id Character))
    , exit : SeqDict (Id Reply) (SeqSet (Id Character))
    }


type MessageId
    = MessageIdReply (Id PostDetails) (Id Reply)
    | MessageIdPost (Id PostDetails)


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    { id : String }


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.preRenderWithFallback
        { head = head
        , data = data
        , pages = BackendTask.succeed []
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


data : RouteParams -> BackendTask FatalError (Response Data ErrorPage)
data params =
    Monad.run (monad params)


monad : RouteParams -> Monad (Response Data ErrorPage)
monad params =
    Do.do
        (case String.toInt params.id of
            Nothing ->
                ("Invalid id: " ++ params.id)
                    |> Monad.failString

            Just i ->
                let
                    boardId : Id PostId
                    boardId =
                        Id.unsafe i
                in
                Monad.succeed boardId
        )
    <| \postId ->
    Do.do (GlowficApi.Extra.getPost postId) <| \post ->
    { name = ""
    , post = post
    , enter = SeqDict.empty
    , exit = SeqDict.empty
    }
        |> Response.render
        |> Monad.succeed


view : RouteBuilder.App Data ActionData RouteParams -> Model -> View (PagesMsg ())
view app model =
    { title = (Tuple.first app.data.post).subject
    , body = [ View.Post.viewThread app.data.post ]
    }
