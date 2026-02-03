module ListHelper exposing (..)


get : Int -> List a -> Maybe a
get index list =
    List.head (List.drop index list)
