module Domain.Icon exposing (..)

import Html exposing (Html)
import Html.Attributes


pencilIcon : Html msg
pencilIcon =
    genericIcon "pencil.svg" 16


checkIcon : Html msg
checkIcon =
    genericIcon "check.svg" 16


closeIcon : Html msg
closeIcon =
    genericIcon "close.svg" 16


resetIcon : Html msg
resetIcon =
    genericIcon "reset.svg" 32


arrowLeftIcon : Html msg
arrowLeftIcon =
    genericIcon "arrow-left.svg" 32


arrowRightIcon : Html msg
arrowRightIcon =
    genericIcon "arrow-right.svg" 32


pizzaIcon : Html msg
pizzaIcon =
    genericIcon "pizza.svg" 32


plusIcon : Int -> Html msg
plusIcon width =
    genericIcon "plus.svg" width


saveIcon : Html msg
saveIcon =
    genericIcon "save.svg" 32


genericIcon : String -> Int -> Html msg
genericIcon path width =
    Html.img
        [ Html.Attributes.width width
        , Html.Attributes.src ("public/img/icon/" ++ path)
        ]
        []
