module Monad exposing (Monad, andThen, combine, durationToString, ensureCacheFolderExists, fail, failString, getRefreshingIf, lift, log, login, map, retryOn429, run, sleepAndLog, succeed, useCachedOn429)

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Do as Do
import BackendTask.Env as Env
import BackendTask.File as File
import BackendTask.Http as Http
import BackendTask.Time as Time
import Dict
import Duration exposing (Duration)
import FatalError exposing (FatalError)
import GlowficApi.Api
import Json.Decode
import Json.Encode
import List.Extra
import Pages.Script as Script
import Quantity
import Time as CoreTime


type Monad a
    = Monad (State -> BackendTask FatalError ( a, State ))


type alias State =
    { token : String
    , got429 : Bool
    }


run : Monad a -> BackendTask FatalError a
run (Monad f) =
    login
        |> BackendTask.andThen f
        |> BackendTask.map Tuple.first


succeed : a -> Monad a
succeed x =
    Monad (\state -> BackendTask.succeed ( x, state ))


map : (a -> b) -> Monad a -> Monad b
map f (Monad x) =
    Monad
        (\init ->
            x init
                |> BackendTask.map
                    (\( y, mid ) ->
                        ( f y, mid )
                    )
        )


andThen : (a -> Monad b) -> Monad a -> Monad b
andThen f (Monad x) =
    Monad
        (\init ->
            x init
                |> BackendTask.andThen
                    (\( y, mid ) ->
                        let
                            (Monad z) =
                                f y
                        in
                        z { token = init.token, got429 = mid.got429 }
                    )
        )


combine : List (Monad a) -> Monad (List a)
combine list =
    Monad
        (\initial ->
            list
                |> List.Extra.greedyGroupsOf 10
                |> List.foldl
                    (\group ->
                        BackendTask.andThen
                            (\( acc, ref ) ->
                                group
                                    |> List.map (\(Monad f) -> f ref)
                                    |> BackendTask.combine
                                    |> BackendTask.map
                                        (\groupResult ->
                                            let
                                                ( f, s ) =
                                                    List.unzip groupResult
                                            in
                                            ( f :: acc, { token = ref.token, got429 = List.any .got429 s } )
                                        )
                            )
                    )
                    (BackendTask.succeed ( [], initial ))
                |> BackendTask.map
                    (\( l, final ) ->
                        ( l
                            |> List.reverse
                            |> List.concat
                        , final
                        )
                    )
        )


failString : String -> Monad a
failString s =
    fail (FatalError.fromString s)


fail : FatalError -> Monad a
fail e =
    Monad (\_ -> BackendTask.fail e)


log : String -> Monad ()
log msg =
    Monad
        (\initial ->
            Script.log msg
                |> BackendTask.map
                    (\() -> ( (), initial ))
        )


lift : BackendTask FatalError a -> Monad a
lift task =
    Monad (\initial -> task |> BackendTask.map (\r -> ( r, initial )))



----------------------------
-- Implementation details --
----------------------------


login : BackendTask FatalError { token : String, got429 : Bool }
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
        |> BackendTask.map (\token -> { token = token, got429 = False })


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


getRefreshingIf :
    (result -> Bool)
    -> String
    ->
        ({ authorization : { authorization : String }, params : params }
         ->
            ( { url : String
              , method : String
              , headers : List ( String, String )
              , body : Http.Body
              , retries : Maybe Int
              , timeoutInMs : Maybe Int
              }
            , Http.Expect result
            )
        )
    -> { params : params }
    -> Monad result
getRefreshingIf refreshCondition label toTuple { params } =
    Monad
        (\({ token, got429 } as initial) ->
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
                    BackendTask.succeed ( res, { token = token, got429 = False } )

                Ok cached ->
                    if not got429 && refreshCondition cached then
                        Do.log (Ansi.Color.fontColor Ansi.Color.cyan ("♻️ Hit refresh condition for " ++ label ++ ", refreshing")) <| \() ->
                        getWith Http.ForceRevalidate
                            |> useCachedOn429 cached initial
                            |> BackendTask.allowFatal

                    else
                        BackendTask.succeed ( cached, initial )
        )


useCachedOn429 :
    a
    -> State
    -> BackendTask { fatal : FatalError, recoverable : Http.Error } a
    -> BackendTask { fatal : FatalError, recoverable : Http.Error } ( a, State )
useCachedOn429 cached initial task =
    BackendTask.onError
        (\({ recoverable } as err) ->
            case recoverable of
                Http.BadStatus metadata _ ->
                    if metadata.statusCode == 429 then
                        Do.log (Ansi.Color.fontColor Ansi.Color.cyan "⚠️ Refreshing failed with a 429, using cached") <| \() ->
                        BackendTask.succeed ( cached, { token = initial.token, got429 = True } )

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
        (task |> BackendTask.map (\r -> ( r, { token = initial.token, got429 = False } )))


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
