module BackendTask.Do.Extra exposing (eachCount)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do


eachCount :
    List a
    -> (a -> BackendTask error b)
    -> (List b -> BackendTask error c)
    -> BackendTask error c
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
                Do.log
                    ("["
                        ++ String.padLeft countLength '0' (String.fromInt (i + 1))
                        ++ "/"
                        ++ countString
                        ++ "]"
                    )
                <| \() ->
                fn v
            )
        |> BackendTask.sequence
        |> BackendTask.andThen then_
