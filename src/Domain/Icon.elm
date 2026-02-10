module Domain.Icon exposing (..)

import Html exposing (Html, s)
import Html.Attributes exposing (attribute)
import Html.Keyed
import Svg exposing (..)
import Svg.Attributes exposing (width)


editIcon : Int -> Html msg
editIcon size =
    genericIcon "pen.svg" size


checkIcon : Int -> Html msg
checkIcon s =
    genericIcon "check.svg" s


menuIcon : Int -> Html msg
menuIcon s =
    genericIcon "menu.svg" s



-- checkIcon : Int -> String -> Html msg
-- checkIcon i color =
--     baseIcon
--         i
--         [ path
--             [ d "M5 13.3636L8.03559 16.3204C8.42388 16.6986 9.04279 16.6986 9.43108 16.3204L19 7"
--             , stroke color
--             , strokeLinecap "round"
--             , strokeLinejoin "round"
--             ]
--             []
--         ]
-- baseIcon : Int -> List (Svg msg) -> Html msg
-- baseIcon i =
--     let
--         size =
--             String.fromInt i
--     in
--     svg
--         [ width size
--         , height size
--         , viewBox (String.join " " [ "0", "0", size, size ])
--         ]


closeIcon : Int -> Html msg
closeIcon size =
    genericIcon "close.svg" size


refreshIcon : Int -> Html msg
refreshIcon size =
    genericIcon "refresh.svg" size


arrowLeftIcon : Int -> Html msg
arrowLeftIcon size =
    genericIcon "left.svg" size


arrowRightIcon : Int -> Html msg
arrowRightIcon size =
    genericIcon "right.svg" size


pizzaIcon : Int -> Html msg
pizzaIcon size =
    genericIcon "pizza.svg" size


addIcon : Int -> Html msg
addIcon size =
    genericIcon "add.svg" size


saveIcon : Int -> Html msg
saveIcon size =
    genericIcon "save.svg" size


genericIcon : String -> Int -> Html msg
genericIcon path width =
    Html.img
        [ Html.Attributes.width width
        , Html.Attributes.src ("public/img/icon/" ++ path)
        ]
        []


ionIcon : String -> Int -> Html msg
ionIcon name size =
    Html.Keyed.node "ion-icon"
        [ attribute "name" name
        , attribute "style" ("font-size:" ++ String.fromInt size ++ "px; vertical-align: middle;")
        ]
        []
