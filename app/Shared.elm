module Shared exposing (Data, Model, Msg(..), SharedMsg, data, init, subscriptions, template, update, view)

import BackendTask exposing (BackendTask)
import Effect exposing (Effect)
import FatalError exposing (FatalError)
import Html exposing (Html)
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import Route exposing (Route)
import SharedTemplate exposing (SharedTemplate)
import UrlPath exposing (UrlPath)
import View exposing (View)


type alias Model =
    {}


type Msg
    = SharedMsg SharedMsg


type alias SharedMsg =
    Never


type alias Data =
    {}


template : SharedTemplate Msg Model Data msg
template =
    { init = init
    , update = update
    , view = view
    , data = data
    , subscriptions = subscriptions
    , onPageChange = Nothing
    }


init :
    Pages.Flags.Flags
    ->
        Maybe
            { path :
                { path : UrlPath
                , query : Maybe String
                , fragment : Maybe String
                }
            , metadata : Maybe route
            , pageUrl : Maybe PageUrl
            }
    -> ( Model, Effect Msg )
init _ _ =
    ( {}, Effect.none )


update : Msg -> Model -> ( Model, Effect Msg )
update msg _ =
    case msg of
        SharedMsg shared ->
            ( never shared, Effect.none )


view :
    Data
    ->
        { path : UrlPath
        , route : Maybe Route
        }
    -> Model
    -> (Msg -> msg)
    -> View msg
    -> { body : List (Html msg), title : String }
view _ _ _ _ pageView =
    { title = pageView.title
    , body = pageView.body
    }


data : BackendTask FatalError Data
data =
    BackendTask.succeed {}


subscriptions : UrlPath -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
