module BackendTask.Do.Extra exposing (..)

import BackendTask exposing (BackendTask)
import BackendTask.Do


eachCount :
    List a
    -> (a -> BackendTask error b)
    -> (List b -> BackendTask error c)
    -> BackendTask error c
eachCount list fn k =
    BackendTask.Do.each list fn k
