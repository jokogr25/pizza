module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, form, img, input, label, span, text)
import Html.Attributes exposing (alt, attribute, class, for, id, placeholder, src, step, style, type_)
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
    | Calculator



-- MSG


type Msg
    = GoToCarousel
    | GoCalculator
    | NoOp



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        GoToCarousel ->
            Carousel

        GoCalculator ->
            Calculator

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

        Calculator ->
            pizzaCalculatorView 1 "none"



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
        [ button
            [ onClick GoToCarousel
            , class "btn btn-primary btn-lg"
            , style "margin-top" "2rem"
            , style "padding" "0.75rem 2rem"
            ]
            [ text "dont we all need someone who looks at us the way joscha looks at pizza 🍕" ]
        , button
            [ onClick GoCalculator
            , class "btn btn-primary btn-lg"
            , style "margin-top" "2rem"
            , style "padding" "0.75rem 2rem"
            ]
            [ text "Pizza calculator" ]
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


pizzaCalculatorView : Float -> String -> Html Msg
pizzaCalculatorView ratio idToEdit =
    div
        [ class "card"
        , style "max-width" "700px"
        , style "margin" "1em auto"
        ]
        [ div
            [ class "card-body"
            ]
            [ Html.h5
                [ class "card-title" ]
                [ text pizza.name ]
            , ingredientView
                "Flour"
                "flourInput"
                Gram
                (pizza.flour * ratio)
                idToEdit
            , ingredientView "Water"
                "waterInput"
                Gram
                (pizza.water * ratio)
                idToEdit
            , ingredientView
                "Yeast"
                "yeastInput"
                Gram
                (pizza.yeast * ratio)
                idToEdit
            , ingredientView
                "Salt"
                "saltInput"
                Gram
                (pizza.salt * ratio)
                idToEdit
            , ingredientView
                "Olive oil"
                "oliveoilInput"
                Mililiter
                (pizza.oliveoil * ratio)
                idToEdit
            , pizzaPrepStepsView pizza.steps
            ]
        ]


ingredientView : String -> String -> Unit -> Float -> String -> Html Msg
ingredientView label id unit value idToEdit =
    div
        [ class "mb-3"
        , style "display" "grid"
        , style "grid-template-columns" "minmax(120px, 1fr) 1fr auto"
        , style "gap" "0.75rem"
        , style "align-items" "center"
        ]
        [ Html.label
            [ for id
            , class "form-label mb-0"
            ]
            [ text label ]
        , input
            [ Html.Attributes.id id
            , type_ "number"
            , class "form-control"
            , Html.Attributes.disabled (id /= idToEdit)
            , placeholder
                (String.fromFloat value ++ " " ++ unitToAbbr unit)
            ]
            []
        , button
            [ type_ "button"
            , class "btn btn-outline-primary"
            ]
            [ text "Edit" ]
        ]


pizzaPrepStepsView : List PrepStep -> Html Msg
pizzaPrepStepsView prepSteps =
    if List.length prepSteps == 0 then
        text "no steps :("

    else
        div
            []
            (List.map pizzaPrepStepView prepSteps)


pizzaPrepStepView : PrepStep -> Html Msg
pizzaPrepStepView prepStep =
    text prepStep.title


type Unit
    = Gram
    | Mililiter


unitToString : Unit -> String
unitToString unit =
    case unit of
        Gram ->
            "grams"

        Mililiter ->
            "mililiters"


unitToAbbr : Unit -> String
unitToAbbr unit =
    case unit of
        Gram ->
            "g"

        Mililiter ->
            "ml"



-- SAMPLE DATA


pizza : Pizza
pizza =
    { name = "7 hours pizza dough"
    , flour = 496
    , water = 313
    , yeast = 3.4
    , oliveoil = 12
    , salt = 15
    , steps = []
    }



-- DOMAIN MODELS


type alias Pizza =
    { name : String
    , flour : Float
    , water : Float
    , yeast : Float
    , oliveoil : Float
    , salt : Float
    , steps : List PrepStep
    }


type alias PrepStep =
    { time : Int
    , title : String
    , description : String
    }
