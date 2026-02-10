module Domain.Icon exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (attribute)
import Html.Keyed
import Svg exposing (..)


ionIcon : String -> Int -> Html msg
ionIcon name size =
    Html.Keyed.node "ion-icon"
        [ attribute "name" name
        , attribute "style" ("font-size:" ++ String.fromInt size ++ "px; vertical-align: middle;")
        ]
        []
