module Id exposing (Id, for, toInt, toString, unsafe)


type Id t
    = Id Int


unsafe : Int -> Id t
unsafe =
    Id


toInt : Id t -> Int
toInt (Id i) =
    i


for : { a | id : Int } -> Id { a | id : Int }
for t =
    Id t.id


toString : Id t -> String
toString (Id i) =
    String.fromInt i
