module SeqDict.Extra exposing (..)

import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)


groupByWith : (a -> k) -> (a -> v) -> List a -> SeqDict k (SeqSet v)
groupByWith toKey toValue list =
    List.foldl
        (\e acc ->
            SeqDict.update (toKey e)
                (\v ->
                    v
                        |> Maybe.withDefault SeqSet.empty
                        |> SeqSet.insert (toValue e)
                        |> Just
                )
                acc
        )
        SeqDict.empty
        list
