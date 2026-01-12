module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, h1, img, span, text)
import Html.Attributes exposing (alt, attribute, class, id, src, style, type_)
import Html.Events exposing (onClick)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = Front
        , update = update
        , view = view
        }



-- MODEL


type Model
    = Front
    | Carousel



-- MSG


type Msg
    = GoToCarousel
    | NoOp



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        GoToCarousel ->
            Carousel

        NoOp ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Front ->
            frontView

        Carousel ->
            viewCarousel1



-- FRONT PAGE


frontView : Html Msg
frontView =
    div
        [ style "height" "100vh"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "text-align" "center"
        , style "padding" "1.5rem"
        ]
        [ h1
            [ class "fw-bold"
            , style "font-size" "clamp(1.8rem, 5vw, 3rem)"
            , style "line-height" "1.2"
            ]
            [ text "dont we all need someone who looks at us the way joscha looks at pizza" ]
        , button
            [ onClick GoToCarousel
            , class "btn btn-primary btn-lg"
            , style "margin-top" "2rem"
            , style "padding" "0.75rem 2rem"
            ]
            [ text "take a look 🍕" ]
        ]


carouselItems : List (Html msg)
carouselItems =
    [ carouselItem True "src/img/IMG_4365.jpeg"
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
            carouselItems
        , if List.length carouselItems > 1 then
            carouselButton "prev" "Previous" "carousel-control-prev" "carousel-control-prev-icon"

          else
            text ""
        , if List.length carouselItems > 1 then
            carouselButton "next" "Next" "carousel-control-next" "carousel-control-next-icon"

          else
            text ""
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
