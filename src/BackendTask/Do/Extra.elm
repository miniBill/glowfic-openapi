module BackendTask.Do.Extra exposing (eachCountWithCircuitBreaker)

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Do as Do
import FatalError exposing (FatalError)
import Json.Decode
import Json.Encode


eachCountWithCircuitBreaker :
    { got429 : Bool }
    -> List a
    -> ({ got429 : Bool } -> a -> BackendTask FatalError ( b, { got429 : Bool } ))
    -> (( List b, { got429 : Bool } ) -> BackendTask FatalError c)
    -> BackendTask FatalError c
eachCountWithCircuitBreaker outer429 list fn then_ =
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
            (\i v r ->
                let
                    msg : String
                    msg =
                        ("[" ++ String.padLeft countLength '0' (String.fromInt (i + 1)) ++ "/" ++ countString ++ "] ")
                            |> Ansi.Color.fontColor (Ansi.Color.rgb { red = 0x90, green = 0x90, blue = 0x90 })
                in
                Do.allowFatal (BackendTask.Custom.run "write" (Json.Encode.string msg) (Json.Decode.succeed ()) |> BackendTask.quiet) <| \() ->
                fn r v
            )
        |> List.foldl
            (\e ->
                BackendTask.andThen
                    (\( a, inner429 ) ->
                        e inner429
                            |> BackendTask.map (\( er, new429 ) -> ( er :: a, new429 ))
                    )
            )
            (BackendTask.succeed ( [], outer429 ))
        |> BackendTask.map (Tuple.mapFirst List.reverse)
        |> BackendTask.andThen then_
