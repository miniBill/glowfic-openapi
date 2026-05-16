module Id exposing (..)


type Id t
    = Id Int


fromInt : Int -> Id t
fromInt =
    Id


toInt : Id t -> Int
toInt (Id i) =
    i
