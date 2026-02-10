module Domain.Helper exposing (..)

import Browser.Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Regex
import Task


empyStyleMapper : Maybe a -> (a -> Html.Attribute msg) -> Html.Attribute msg
empyStyleMapper m f =
    m
        |> Maybe.map f
        |> Maybe.withDefault emptyStyle


emptyStyle : Html.Attribute msg
emptyStyle =
    Html.Attributes.style "" ""


focus : String -> msg -> Cmd msg
focus id msg =
    Browser.Dom.focus id
        |> Task.attempt (\_ -> msg)


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


uniqueStrings : List String -> Bool
uniqueStrings list =
    List.length list
        == List.length
            (List.foldl
                (\id unique ->
                    if List.member id unique then
                        unique

                    else
                        unique ++ [ id ]
                )
                []
                list
            )
