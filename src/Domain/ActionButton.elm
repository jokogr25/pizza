module Domain.ActionButton exposing (..)

import Domain.Icon exposing (ionIcon)
import Html exposing (Html, button)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)


type ActionButton msg
    = Pre msg Bool
    | Nex msg Bool
    | Save msg Bool
    | Refresh msg Bool


actionToIcon : ActionButton msg -> Html msg
actionToIcon action =
    let
        ( icon, message, isDisabled ) =
            case action of
                Pre m d ->
                    ( "chevron-back-outline", m, d )

                Nex m d ->
                    ( "chevron-forward-outline", m, d )

                Save m d ->
                    ( "save", m, d )

                Refresh m d ->
                    ( "refresh", m, d )
    in
    button
        [ class "btn"
        , onClick message
        , disabled isDisabled
        ]
        [ ionIcon icon 32
        ]
