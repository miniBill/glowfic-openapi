module Route.Index exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import BackendTask.Env as Env
import FatalError exposing (FatalError)
import GlowficApi.Api
import GlowficApi.Json
import GlowficApi.Types exposing (Post)
import Head
import Head.Seo as Seo
import Html
import Json.Encode
import Pages.Url
import PagesMsg exposing (PagesMsg)
import RouteBuilder exposing (App, StatelessRoute)
import UrlPath
import View exposing (View)


type alias ActionData =
    Never


type alias Data =
    Post


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


data : BackendTask FatalError Data
data =
    BackendTask.map2 Tuple.pair
        (Env.expect "username")
        (Env.expect "password")
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\( username, password ) ->
                GlowficApi.Api.login
                    { body =
                        { username = username
                        , password = password
                        }
                    }
                    |> BackendTask.allowFatal
            )
        |> BackendTask.andThen
            (\{ token } ->
                GlowficApi.Api.postsId
                    { authorization =
                        { authorization = token
                        }
                    , params = { id = 47527 }
                    }
                    |> BackendTask.allowFatal
            )


view : App Data ActionData {} -> Model -> View (PagesMsg ())
view app _ =
    { title = "Chaser Six When?"
    , body =
        [ Html.pre []
            [ app.data
                |> GlowficApi.Json.encodePost
                |> Json.Encode.encode 4
                |> Html.text
            ]
        ]
    }
