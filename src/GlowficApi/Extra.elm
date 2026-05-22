module GlowficApi.Extra exposing (getAllBoardsIdPosts, getBoard, getCharacter, getPost, login)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Env as Env
import BackendTask.File as File
import BackendTask.Http as Http exposing (Body, Expect)
import BackendTask.Time as Time
import Dict
import FatalError exposing (FatalError)
import GlowficApi.Api
import GlowficApi.Json
import GlowficApi.Types exposing (Board, Character, CharacterDetails, Icon, PostDetails, PostSummary, Reply)
import Id exposing (Id(..))
import Json.Decode
import OpenApi.Common
import Pages.Script as Script
import Time as CoreTime
import Triple.Extra exposing (from)
import Url exposing (Url)


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
                    |> retryOn429
                    |> BackendTask.allowFatal
            )


retryOn429 :
    BackendTask { a | recoverable : Http.Error } b
    -> BackendTask { a | recoverable : Http.Error } b
retryOn429 task =
    BackendTask.onError
        (\({ recoverable } as err) ->
            case recoverable of
                Http.BadStatus metadata body ->
                    case Dict.get "ratelimit-reset" metadata.headers |> Maybe.andThen String.toInt of
                        Just resetAt ->
                            Do.do Time.now <| \now ->
                            let
                                delta : Int
                                delta =
                                    resetAt * 1000 - CoreTime.posixToMillis now
                            in
                            Do.log ("Hit a 429, sleeping for " ++ String.fromInt (delta // 1000) ++ "s") <| \() ->
                            Do.do (Script.sleep delta) <| \() ->
                            task

                        Nothing ->
                            BackendTask.fail err

                _ ->
                    BackendTask.fail err
        )
        task


getCharacter : { token : String } -> Id t -> BackendTask FatalError CharacterDetails
getCharacter authorization (Id id) =
    getCachedWithAuthorization authorization
        GlowficApi.Api.getCharactersIdRecord
        { params =
            { id = id
            , post_id = Nothing
            }
        }


getPost : { token : String } -> Id PostDetails -> BackendTask FatalError ( PostDetails, List Reply )
getPost authorization (Id id) =
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


getBoard :
    { token : String }
    -> Id Board
    ->
        BackendTask
            FatalError
            { id : Int
            , name : String
            , board_sections : List { id : Int, name : String, order : Int }
            }
getBoard authorization (Id continuityId) =
    getCachedWithAuthorization authorization
        GlowficApi.Api.getBoardsIdRecord
        { params = { id = continuityId } }


getAllBoardsIdPosts :
    { token : String }
    -> Id Board
    -> BackendTask FatalError (List PostSummary)
getAllBoardsIdPosts authorization (Id continuityId) =
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
            Script.writeFile
                { path = ".elm-pages/http-response-cache/keep"
                , body = ""
                }

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
        |> retryOn429
        |> BackendTask.allowFatal
