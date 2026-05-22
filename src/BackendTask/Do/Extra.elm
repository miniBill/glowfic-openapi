module BackendTask.Do.Extra exposing (eachCount)

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Do as Do
import FatalError exposing (FatalError)
import Json.Decode
import Json.Encode


eachCount :
    List a
    -> (a -> BackendTask FatalError b)
    -> (List b -> BackendTask FatalError c)
    -> BackendTask FatalError c
eachCount list fn then_ =
    let
        count : Int
        count =
            List.length list

        countString : String
        countString =
            String.fromInt count

        countLength : Int
        countLength =
            String.length countString
    in
    list
        |> List.indexedMap
            (\i v ->
                let
                    msg : String
                    msg =
                        ("[" ++ String.padLeft countLength '0' (String.fromInt (i + 1)) ++ "/" ++ countString ++ "] ")
                            |> Ansi.Color.fontColor (Ansi.Color.rgb { red = 0x90, green = 0x90, blue = 0x90 })
                in
                Do.allowFatal (BackendTask.Custom.run "write" (Json.Encode.string msg) (Json.Decode.succeed ()) |> BackendTask.quiet) <| \() ->
                fn v
            )
        |> BackendTask.sequence
        |> BackendTask.andThen then_
