module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (alt, attribute, class, id, src, style, type_)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = Nille
        , update = update
        , view = view
        }


type Model
    = Nille


type Msg
    = NoOp


update : Msg -> Model -> Model
update _ model =
    model


view : Model -> Html Msg
view _ =
    viewCarousel1


viewCarousel : Html msg
viewCarousel =
    div
        [ id "carouselExample"
        , class "carousel slide h-25"
        ]
        [ div
            [ class "carousel-inner" ]
            [ div
                [ class "carousel-item active" ]
                [ img
                    [ src "src/img/IMG_4365.jpeg"
                    , class "d-block h-25"
                    , alt "joscha looking at pizza"
                    ]
                    []
                ]
            , div
                [ class "carousel-item" ]
                [ img
                    [ src "src/img/IMG_4365.jpeg"
                    , class "d-block h-25"
                    , alt "joscha looking at pizza"
                    ]
                    []
                ]
            ]
        , button
            [ class "carousel-control-prev"
            , type_ "button"
            , attribute "data-bs-target" "#carouselExample"
            , attribute "data-bs-slide" "prev"
            ]
            [ span
                [ class "carousel-control-prev-icon"
                , attribute "aria-hidden" "true"
                ]
                []
            , span [ class "visually-hidden" ] [ text "Previous" ]
            ]
        , button
            [ class "carousel-control-next"
            , type_ "button"
            , attribute "data-bs-target" "#carouselExample"
            , attribute "data-bs-slide" "next"
            ]
            [ span
                [ class "carousel-control-next-icon"
                , attribute "aria-hidden" "true"
                ]
                []
            , span [ class "visually-hidden" ] [ text "Next" ]
            ]
        ]


viewCarousel1 : Html msg
viewCarousel1 =
    div
        [ id "carouselExample"
        , class "carousel slide"
        , style "height" "100vh"
        ]
        [ div
            [ class "carousel-inner"
            , style "height" "100%"
            ]
            [ carouselItem True "src/img/IMG_4365.jpeg"
            ]
        , carouselButton "prev" "Previous" "carousel-control-prev" "carousel-control-prev-icon"
        , carouselButton "next" "Next" "carousel-control-next" "carousel-control-next-icon"
        ]


carouselItem : Bool -> String -> Html msg
carouselItem isActive imageSrc =
    div
        [ class
            (if isActive then
                "carousel-item active"

             else
                "carousel-item"
            )
        , style "height" "100%"
        , style "background-color" "#111"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        ]
        [ img
            [ src imageSrc
            , alt ""
            , class "d-block w-100"
            , style "height" "100%"
            , style "object-fit" "contain"
            ]
            []
        ]


carouselButton : String -> String -> String -> String -> Html msg
carouselButton direction label btnClass iconClass =
    button
        [ class btnClass
        , type_ "button"
        , attribute "data-bs-target" "#carouselExample"
        , attribute "data-bs-slide" direction
        ]
        [ span
            [ class iconClass
            , attribute "aria-hidden" "true"
            ]
            []
        , span [ class "visually-hidden" ] [ text label ]
        ]
