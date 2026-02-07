module Helper exposing (..)


get : Int -> List a -> Maybe a
get index list =
    List.head (List.drop index list)


round2ToString : Float -> String
round2ToString x =
    let
        rounded =
            toFloat (round (x * 100)) / 100.0
    in
    String.fromFloat rounded
