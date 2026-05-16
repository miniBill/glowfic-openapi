module GlowficApi.Extra exposing (getAllBoardsIdPosts, getPost, login)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Env as Env
import BackendTask.File as File
import BackendTask.Http as Http exposing (Body, Expect)
import FatalError exposing (FatalError)
import GlowficApi.Api
import GlowficApi.Json
import GlowficApi.Types exposing (PostDetails, PostSummary, Reply)
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


getAllBoardsIdPosts :
    { token : String }
    -> Int
    -> BackendTask FatalError (List PostSummary)
getAllBoardsIdPosts authorization continuityId =
    let
        go : Int -> List (List PostSummary) -> BackendTask FatalError (List PostSummary)
        go page acc =
            getCachedWithAuthorization authorization
                GlowficApi.Api.getBoardsIdPostsRecord
                { params =
                    { id = continuityId
                    , page = Just page
                    }
                }
                |> BackendTask.andThen
                    (\{ results } ->
                        if List.isEmpty results then
                            acc
                                |> List.reverse
                                |> List.concat
                                |> BackendTask.succeed

                        else
                            go (page + 1) (results :: acc)
                    )
    in
    go 1 []


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
    Do.do (File.exists ".elm-pages/http-response-cache/keep") <| \exists ->
    Do.allowFatal
        (if not exists then
            Script.writeFile { path = ".elm-pages/http-response-cache/keep", body = "" }

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
        , cachePath = Just ".elm-pages/http-response-cache"
        , cacheStrategy = Just Http.ForceCache
        }
        |> BackendTask.allowFatal
