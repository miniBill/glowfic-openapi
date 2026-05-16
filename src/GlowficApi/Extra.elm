module GlowficApi.Extra exposing (getPost, login)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Env as Env
import BackendTask.File as File
import BackendTask.Http as Http exposing (Body, Expect)
import FatalError exposing (FatalError)
import GlowficApi.Api
import GlowficApi.Json
import GlowficApi.Types exposing (PostDetails, Reply)
import Json.Decode
import Pages.Script as Script
import Triple.Extra exposing (from)


login : BackendTask FatalError { token : String }
login =
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


getPost : { token : String } -> Int -> BackendTask FatalError ( PostDetails, List Reply )
getPost authorization id =
    getCachedWithAuthorization authorization
        GlowficApi.Api.getPostsIdRecord
        { params = { id = id }
        }
        |> BackendTask.andThen
            (\post ->
                List.range 0 ((post.num_replies - 1) // 100)
                    |> List.map
                        (\page ->
                            getCachedWithAuthorization authorization
                                GlowficApi.Api.getPostsIdRepliesRecord
                                { params =
                                    { id = id
                                    , page = Just page
                                    , per_page = Just 100
                                    }
                                }
                        )
                    |> BackendTask.combine
                    |> BackendTask.map List.concat
                    |> BackendTask.map (\replies -> ( post, replies ))
            )


getCachedWithAuthorization :
    { token : String }
    ->
        ({ authorization : { authorization : String }, params : params }
         ->
            ( { url : String
              , method : String
              , headers : List ( String, String )
              , body : Body
              , retries : Maybe Int
              , timeoutInMs : Maybe Int
              }
            , Expect result
            )
        )
    -> { params : params }
    -> BackendTask FatalError result
getCachedWithAuthorization { token } toTuple { params } =
    let
        ( record, expect ) =
            toTuple
                { authorization = { authorization = token }
                , params = params
                }
    in
    Do.do (File.exists ".cache/keep") <| \exists ->
    Do.allowFatal
        (if not exists then
            Script.writeFile { path = ".cache/keep", body = "" }

         else
            BackendTask.succeed ()
        )
    <| \_ ->
    Http.getWithOptions
        { url = record.url
        , expect = expect
        , retries = record.retries
        , timeoutInMs = record.timeoutInMs
        , headers = record.headers
        , cachePath = Just ".cache"
        , cacheStrategy = Just Http.ForceCache
        }
        |> BackendTask.allowFatal
