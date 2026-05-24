module Monad.Do exposing (do, eachCount, log)

import Ansi.Color
import BackendTask
import BackendTask.Custom
import Json.Decode
import Json.Encode
import Monad exposing (Monad)


log : String -> (() -> Monad a) -> Monad a
log msg then_ =
    do (Monad.log msg) then_


do : Monad a -> (a -> Monad b) -> Monad b
do x f =
    Monad.andThen f x


eachCount : List a -> (a -> Monad b) -> (List b -> Monad c) -> Monad c
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
                do (write msg) <| \() ->
                fn v
            )
        |> List.foldl
            (\e ->
                Monad.andThen
                    (\a ->
                        e
                            |> Monad.map (\er -> er :: a)
                    )
            )
            (Monad.succeed [])
        |> Monad.map List.reverse
        |> Monad.andThen then_


write : String -> Monad ()
write msg =
    Monad.lift
        (BackendTask.Custom.run "write"
            (Json.Encode.string msg)
            (Json.Decode.succeed ())
            |> BackendTask.quiet
            |> BackendTask.allowFatal
        )
