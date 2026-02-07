module Helper exposing (..)

import Regex


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


unwords : List String -> String
unwords wordsList =
    String.join " " wordsList


safeRegexOf : String -> Regex.Regex
safeRegexOf s =
    Maybe.withDefault Regex.never <|
        Regex.fromString s
