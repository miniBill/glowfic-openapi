module GlowficApi.Extra exposing (..)

import BackendTask exposing (BackendTask)
import BackendTask.Env as Env
import FatalError exposing (FatalError)
import GlowficApi.Api


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
