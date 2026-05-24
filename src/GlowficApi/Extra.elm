module GlowficApi.Extra exposing (getAllBoardsIdPosts, getBoard, getCharacter, getPost, login)

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Env as Env
import BackendTask.File as File
import BackendTask.Http as Http exposing (Body, Expect)
import BackendTask.Time as Time
import Dict
import Duration exposing (Duration)
import FatalError exposing (FatalError)
import GlowficApi.Api
import GlowficApi.Types exposing (Board, CharacterDetails, PostDetails, PostSummary, Reply)
import Id exposing (BoardId, CharacterId, Id, PostId, SectionId)
import List.Extra
import Pages.Script as Script
import Quantity
import Time as CoreTime


login : BackendTask FatalError ( { token : String }, { got429 : Bool } )
login =
    let
        tokenPath : String
        tokenPath =
            ".elm-pages/http-response-cache/token"
    in
    File.rawFile tokenPath
        |> BackendTask.allowFatal
        |> BackendTask.onError
            (\_ ->
                Do.allowFatal
                    (BackendTask.map2 Tuple.pair
                        (Env.expect "username")
                        (Env.expect "password")
                    )
                <| \( username, password ) ->
                Do.allowFatal
                    (GlowficApi.Api.login
                        { body =
                            { username = username
                            , password = password
                            }
                        }
                        |> retryOn429 10
                    )
                <| \auth ->
                Do.allowFatal (Script.writeFile { path = tokenPath, body = auth.token }) <| \_ ->
                BackendTask.succeed auth.token
            )
        |> BackendTask.map (\token -> ( { token = token }, { got429 = False } ))


retryOn429 :
    Int
    -> BackendTask { a | recoverable : Http.Error } b
    -> BackendTask { a | recoverable : Http.Error } b
retryOn429 budget task =
    if budget <= 1 then
        task

    else
        BackendTask.onError
            (\({ recoverable } as err) ->
                case recoverable of
                    Http.BadStatus metadata _ ->
                        case
                            Dict.get "ratelimit-reset" metadata.headers
                                |> Maybe.andThen String.toInt
                                |> Maybe.map (\seconds -> CoreTime.millisToPosix (seconds * 1000))
                        of
                            Just resetAt ->
                                Do.do Time.now <| \now ->
                                let
                                    delta : Duration
                                    delta =
                                        Duration.from now resetAt
                                in
                                Do.log (Ansi.Color.fontColor Ansi.Color.cyan ("💤 Hit a 429, sleeping for " ++ durationToString delta)) <| \() ->
                                Do.do (sleepAndLog delta) <| \() ->
                                retryOn429 (budget - 1) task

                            Nothing ->
                                BackendTask.fail err

                    Http.BadUrl _ ->
                        BackendTask.fail err

                    Http.Timeout ->
                        BackendTask.fail err

                    Http.NetworkError ->
                        BackendTask.fail err

                    Http.BadBody _ _ ->
                        BackendTask.fail err
            )
            task


sleepAndLog : Duration -> BackendTask e ()
sleepAndLog milliseconds =
    if milliseconds |> Quantity.lessThanOrEqualTo Duration.second then
        Script.sleep (round (Duration.inMilliseconds milliseconds))

    else
        let
            toSleep : Duration
            toSleep =
                milliseconds
                    |> Quantity.divideBy 10
                    |> Quantity.max Duration.second
                    |> Quantity.min milliseconds

            left : Duration
            left =
                milliseconds |> Quantity.minus toSleep
        in
        Do.log (" - " ++ durationToString milliseconds ++ " left") <| \() ->
        Do.do (Script.sleep (round (Duration.inMilliseconds toSleep))) <| \() ->
        sleepAndLog left


durationToString : Duration -> String
durationToString duration =
    let
        milliseconds : Int
        milliseconds =
            round (Duration.inMilliseconds duration)

        day : Int
        day =
            24 * hour

        hour : Int
        hour =
            60 * minute

        minute : Int
        minute =
            60 * second

        second : Int
        second =
            1000
    in
    if milliseconds >= day * 10 then
        String.fromInt (milliseconds // day) ++ "d"

    else if milliseconds >= day then
        String.fromInt (milliseconds // day) ++ "d " ++ String.padLeft 2 '0' (String.fromInt (milliseconds // hour |> modBy 24)) ++ "h"

    else if milliseconds >= hour then
        String.fromInt (milliseconds // hour) ++ "h " ++ String.padLeft 2 '0' (String.fromInt (milliseconds // minute |> modBy 60)) ++ "m"

    else if milliseconds >= minute then
        String.fromInt (milliseconds // minute) ++ "m " ++ String.padLeft 2 '0' (String.fromInt (milliseconds // second |> modBy 60)) ++ "s"

    else if milliseconds >= 10 * second then
        String.fromInt (milliseconds // second) ++ "." ++ String.padLeft 1 '0' (String.fromInt ((milliseconds |> modBy 1000) // 100)) ++ "s"

    else if milliseconds >= second then
        String.fromInt (milliseconds // second) ++ "." ++ String.padLeft 3 '0' (String.fromInt (milliseconds |> modBy 1000)) ++ "s"

    else
        String.fromInt milliseconds ++ "ms"


getCharacter :
    { got429 : Bool }
    -> { token : String }
    -> Id CharacterId
    -> BackendTask FatalError ( CharacterDetails, { got429 : Bool } )
getCharacter got429 authorization id =
    getRefreshingIf got429
        (\_ -> False)
        ("getCharacter " ++ Id.toString id)
        GlowficApi.Api.getCharactersIdRecord
        authorization
        { params =
            { id = id
            , post_id = Nothing
            }
        }


getPost :
    { got429 : Bool }
    -> { token : String }
    -> Id PostId
    -> BackendTask FatalError ( ( PostDetails, List Reply ), { got429 : Bool } )
getPost got429 authorization id =
    getRefreshingIf got429
        (\post -> post.status /= GlowficApi.Types.Status__Complete)
        ("getPost " ++ Id.toString id)
        GlowficApi.Api.getPostsIdRecord
        authorization
        { params = { id = id }
        }
        |> BackendTask.andThen
            (\( post, newTryRefresh ) ->
                let
                    maxPage : Int
                    maxPage =
                        ((post.num_replies - 1) // 100) + 1
                in
                List.range 1 maxPage
                    |> List.map
                        (\page innerGot429 ->
                            getRefreshingIf innerGot429
                                (\_ -> page == maxPage && post.status /= GlowficApi.Types.Status__Complete)
                                ("getPost " ++ Id.toString id ++ " page " ++ String.fromInt page)
                                GlowficApi.Api.getPostsIdRepliesRecord
                                authorization
                                { params =
                                    { id = id
                                    , page = Just page
                                    , per_page = Just 100
                                    }
                                }
                        )
                    |> combineWithCircuitBreaker newTryRefresh
                    |> BackendTask.map
                        (\( replies, nextTryRefresh ) ->
                            ( ( post, List.concat replies ), nextTryRefresh )
                        )
            )


combineWithCircuitBreaker :
    { got429 : Bool }
    ->
        List
            ({ got429 : Bool }
             -> BackendTask FatalError ( List Reply, { got429 : Bool } )
            )
    -> BackendTask FatalError ( List (List Reply), { got429 : Bool } )
combineWithCircuitBreaker got429 list =
    list
        |> List.Extra.greedyGroupsOf 10
        |> List.foldl
            (\group prev ->
                prev
                    |> BackendTask.andThen
                        (\( acc, ref ) ->
                            List.map (\f -> f ref) group
                                |> BackendTask.combine
                                |> BackendTask.map
                                    (\groupResult ->
                                        let
                                            ( f, s ) =
                                                List.unzip groupResult
                                        in
                                        ( f :: acc, { got429 = List.any .got429 s } )
                                    )
                        )
            )
            (BackendTask.succeed ( [], got429 ))
        |> BackendTask.map
            (\( l, got ) ->
                ( l
                    |> List.reverse
                    |> List.concat
                , got
                )
            )


getBoard :
    { got429 : Bool }
    -> { token : String }
    -> Id BoardId
    ->
        BackendTask
            FatalError
            ( { id : Id BoardId
              , name : String
              , board_sections : List { id : Id SectionId, name : String, order : Int }
              }
            , { got429 : Bool }
            )
getBoard got429 authorization id =
    getRefreshingIf got429
        (\_ -> True)
        ("getBoard " ++ Id.toString id)
        GlowficApi.Api.getBoardsIdRecord
        authorization
        { params = { id = id } }


getAllBoardsIdPosts :
    { got429 : Bool }
    -> { token : String }
    -> Id BoardId
    -> BackendTask FatalError ( List PostSummary, { got429 : Bool } )
getAllBoardsIdPosts got429 authorization continuityId =
    let
        go :
            { got429 : Bool }
            -> Int
            -> List (List PostSummary)
            -> BackendTask FatalError ( List PostSummary, { got429 : Bool } )
        go innerRefresh page acc =
            getRefreshingIf innerRefresh
                (\_ -> True)
                ("getAllBoardsIdPosts " ++ Id.toString continuityId)
                GlowficApi.Api.getBoardsIdPostsRecord
                authorization
                { params =
                    { id = continuityId
                    , page = Just page
                    }
                }
                |> BackendTask.andThen
                    (\( { results }, newInnerRefresh ) ->
                        if List.isEmpty results then
                            ( acc
                                |> List.reverse
                                |> List.concat
                            , newInnerRefresh
                            )
                                |> BackendTask.succeed

                        else
                            go newInnerRefresh (page + 1) (results :: acc)
                    )
    in
    go got429 1 []


getRefreshingIf :
    { got429 : Bool }
    -> (result -> Bool)
    -> String
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
    -> { token : String }
    -> { params : params }
    -> BackendTask FatalError ( result, { got429 : Bool } )
getRefreshingIf { got429 } refreshCondition label toTuple { token } { params } =
    let
        ( record, expect ) =
            toTuple
                { authorization = { authorization = token }
                , params = params
                }

        getWith : Http.CacheStrategy -> BackendTask { fatal : FatalError, recoverable : Http.Error } result
        getWith cachePolicy =
            Http.getWithOptions
                { url = record.url
                , expect = expect
                , retries = record.retries
                , timeoutInMs = record.timeoutInMs
                , headers = record.headers
                , cachePath = Just ".elm-pages/http-response-cache"
                , cacheStrategy = Just cachePolicy
                }
    in
    Do.allowFatal ensureCacheFolderExists <| \_ ->
    Do.do (getWith Http.ForceCache |> BackendTask.toResult) <| \fromCache ->
    case fromCache of
        Err _ ->
            -- Not in cache, refresh
            Do.allowFatal (retryOn429 10 (getWith Http.ForceRevalidate)) <| \res ->
            -- Counterintuitively, if we got a 429 we set this to False anyway
            -- because by the time retryOn429 finishes we're out of the retry loop
            BackendTask.succeed ( res, { got429 = False } )

        Ok cached ->
            if not got429 && refreshCondition cached then
                Do.log (Ansi.Color.fontColor Ansi.Color.cyan ("♻️ Hit refresh condition for " ++ label ++ ", refreshing")) <| \() ->
                getWith Http.ForceRevalidate
                    |> useCachedOn429 cached
                    |> BackendTask.allowFatal

            else
                BackendTask.succeed ( cached, { got429 = got429 } )


useCachedOn429 :
    a
    -> BackendTask { fatal : FatalError, recoverable : Http.Error } a
    -> BackendTask { fatal : FatalError, recoverable : Http.Error } ( a, { got429 : Bool } )
useCachedOn429 cached task =
    BackendTask.onError
        (\({ recoverable } as err) ->
            case recoverable of
                Http.BadStatus metadata _ ->
                    if metadata.statusCode == 429 then
                        Do.log (Ansi.Color.fontColor Ansi.Color.cyan "⚠️ Refreshing failed with a 429, using cached") <| \() ->
                        BackendTask.succeed ( cached, { got429 = True } )

                    else
                        BackendTask.fail err

                Http.BadUrl _ ->
                    BackendTask.fail err

                Http.Timeout ->
                    BackendTask.fail err

                Http.NetworkError ->
                    BackendTask.fail err

                Http.BadBody _ _ ->
                    BackendTask.fail err
        )
        (task |> BackendTask.map (\r -> ( r, { got429 = False } )))


ensureCacheFolderExists : BackendTask { fatal : FatalError, recoverable : Script.Error } ()
ensureCacheFolderExists =
    Do.do (File.exists ".elm-pages/http-response-cache/keep") <| \exists ->
    if not exists then
        Script.writeFile
            { path = ".elm-pages/http-response-cache/keep"
            , body = ""
            }

    else
        BackendTask.succeed ()
