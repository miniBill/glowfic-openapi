module Route.MCU exposing (..)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Http as Http
import Dict exposing (Dict)
import FatalError exposing (FatalError)
import GlowficApi.Api
import GlowficApi.Extra
import GlowficApi.Types exposing (PostDetails, PostSummary, Reply)
import Head
import Head.Seo as Seo
import Html
import Pages.Url
import PagesMsg exposing (PagesMsg)
import RouteBuilder exposing (App, StatelessRoute)
import UrlPath
import View exposing (View)


type alias ActionData =
    Never


type alias Data =
    Dict Int ( PostDetails, List Reply )


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


continuityId : Int
continuityId =
    4968


data : BackendTask FatalError Data
data =
    Do.do GlowficApi.Extra.login <| \token ->
    Do.do (GlowficApi.Extra.getAllBoardsIdPosts token continuityId) <| \results ->
    Do.each results (\{ id } -> GlowficApi.Extra.getPost token id) <| \posts ->
    posts
        |> List.map (\( p, r ) -> ( p.id, ( p, r ) ))
        |> Dict.fromList
        |> BackendTask.succeed


view : App Data ActionData {} -> Model -> View (PagesMsg ())
view app model =
    { title = "MCU"
    , body =
        app.data
            |> Dict.toList
            |> List.map
                (\d ->
                    Html.li [] [ Html.text (Debug.toString d) ]
                )
            |> Html.ul []
            |> List.singleton
    }
