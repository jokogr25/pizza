module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, img, input, label, span, text)
import Html.Attributes exposing (alt, attribute, class, disabled, id, placeholder, src, style, type_)
import Html.Events exposing (onClick)
import ListHelper
import Svg
import Svg.Attributes



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
    | Calculator Int Float String



-- MSG


type Msg
    = GoCarousel
    | GoCalculator
    | Edit String
    | Next
    | Prev
    | NoOp



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        GoCarousel ->
            Carousel

        GoCalculator ->
            Calculator 0 1 "none"

        Edit idToEdit ->
            case model of
                Calculator index ratio _ ->
                    Calculator index ratio idToEdit

                _ ->
                    model

        Next ->
            case model of
                Calculator index ratio idToEdit ->
                    Calculator
                        (index + 1)
                        ratio
                        idToEdit

                _ ->
                    model

        Prev ->
            case model of
                Calculator index ratio idToEdit ->
                    Calculator
                        (max 0 (index - 1))
                        ratio
                        idToEdit

                _ ->
                    model

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

        Calculator stepIndex ratio idToEdit ->
            pizzaCalculatorView stepIndex ratio idToEdit



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
            [ onClick GoCarousel
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
            [ text "🍕 🧮" ]
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


pizzaCalculatorView : Int -> Float -> String -> Html Msg
pizzaCalculatorView stepIndex ratio idToEdit =
    div
        [ class "card"
        , style "max-width" "700px"
        , style "margin" "1em auto"
        ]
        [ div
            [ class "card-body"
            ]
            [ Html.h2
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
            , ingredientView
                "Honey"
                "honeyInput"
                Teaspoon
                (toFloat pizza.honey * ratio)
                idToEdit
            , pizzaPrepStepsView stepIndex pizza.steps
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
        [ text label
        , input
            [ Html.Attributes.id id
            , type_ "number"
            , class "form-control"
            , placeholder (String.fromFloat value ++ " " ++ unitToAbbr unit)
            , disabled (id /= idToEdit)
            ]
            []
        , if id == idToEdit then
            button
                [ type_ "button"
                , class "btn btn-primary"
                , onClick (Edit "none")
                ]
                [ img
                    [ Html.Attributes.width 16
                    , src "src/img/icon/check.svg"
                    ]
                    []
                ]

          else
            button
                [ type_ "button"
                , class "btn btn-primary"
                , onClick (Edit id)
                ]
                [ img
                    [ Html.Attributes.width 16
                    , src "src/img/icon/pencil.svg"
                    ]
                    []
                ]
        ]


pizzaPrepStepsView : Int -> List PrepStep -> Html Msg
pizzaPrepStepsView index prepSteps =
    if List.length prepSteps == 0 then
        text "no steps :("

    else
        div
            []
            [ case ListHelper.get index prepSteps of
                Just prepStep ->
                    pizzaPrepStepView index prepStep

                Nothing ->
                    text "ERROR"
            ]


pizzaPrepStepView : Int -> PrepStep -> Html Msg
pizzaPrepStepView i prepStep =
    div
        [ Html.Attributes.style "margin-top" "1rem" ]
        [ Html.h3
            []
            [ text (String.fromInt (i + 1) ++ ". " ++ prepStep.title) ]
        , div
            []
            [ if prepStep.time == -1 then
                text "∞"

              else if prepStep.time == 0 then
                text ""

              else
                text (String.fromInt prepStep.time ++ " mins")
            ]
        , div
            []
            [ text prepStep.description ]
        , div
            [ class "mb-3"
            , style "display" "grid"
            , style "grid-template-columns" "1fr 1fr"
            , style "gap" "0.75rem"
            ]
            [ button
                [ onClick Prev
                , class "btn btn-primary btn-lg"
                , style "margin-top" "2rem"
                , style "padding" "0.75rem 2rem"
                ]
                [ text "<--" ]
            , button
                [ onClick Next
                , disabled (i >= List.length pizza.steps - 1)
                , class "btn btn-primary btn-lg"
                , style "margin-top" "2rem"
                , style "padding" "0.75rem 2rem"
                ]
                [ text "-->" ]
            ]
        ]


type Unit
    = Gram
    | Mililiter
    | Teaspoon



{-
   unitToString : Unit -> String
   unitToString unit =
       case unit of
           Gram ->
               "grams"

           Mililiter ->
               "mililiters"

           Teaspoon ->
               "teaspoon"
-}


unitToAbbr : Unit -> String
unitToAbbr unit =
    case unit of
        Gram ->
            "g"

        Mililiter ->
            "ml"

        Teaspoon ->
            "tsp"



-- SAMPLE DATA


pizza : Pizza
pizza =
    { name = "7 hours pizza dough"
    , flour = 496
    , water = 313
    , yeast = 3.4
    , oliveoil = 12
    , honey = 1
    , salt = 15
    , steps =
        [ { time = 15
          , title = "Pre mix"
          , description = "Mix flour and roughly π/4 of water in a bowl, leave it."
          }
        , { time = 15
          , title = "BRING THE YEAST TO LIFE"
          , description = "Mix rest of the water with yeast and honey, leave it."
          }
        , { time = 10
          , title = "imx"
          , description = "Put all ingredients to flour/water bowl and knead, as if your life depends on it. The dough is ready, when it stops sticking to bowl and hands"
          }
        , { time = 7 * 60
          , title = "slumber time"
          , description = "Put the dough in an airtight box in the fridge and LET IT GOOoOOOOOoooooooo"
          }
        , { time = 5
          , title = "Roll it, baby"
          , description = "Portion dough into 5-6 parts (~140-170g per roll) and roll each to a smoooooth ball."
          }
        , { time = 60
          , title = "stueckgare"
          , description = "After this stressful first hours in life, each of the pizza balls needs to rest separated from their siblings, to meditate and grow, question existence, in an (almost) airtight box."
          }
        , { time = 5
          , title = "MAX POWER"
          , description = "Pre-heat oven to max"
          }
        , { time = 5
          , title = "Don't we all need a little stretch when we're older?"
          , description = "Put some semola on a clean and smooooth surface, carefully put one ball on the semola (in their current state they're very sensitive, so be really cautious) and stretch it from the inner to the outer in a circling motion. we want it shallow on the inside and thick on the edge"
          }
        , { time = 5
          , title = "What belongs together, will be together in the end"
          , description = "Add tomate sauce, cheese and everything else you like. Yes, pineapple is allowed. No, hollandaise is not, get over it. It's BLASFEMIA. Do it and I'll call the cops"
          }
        , { time = 0
          , title = "Ich bin nicht sauer, ich bin enttäuscht"
          , description = "You did it, right? That's okay. Pizza is for everyone, even taste-impaired germans."
          }
        , { time = 0
          , title = "CIIIIIIRCLEE OF LIIIIFEE"
          , description = "Put pizza in oven until cheese starts bubbling and the circle of life gets a little color"
          }
        , { time = 0
          , title = "Enjoy"
          , description = "You need an instruction for that too?"
          }
        , { time = -1
          , title = "I knew it"
          , description = "Call some friends, your parents, grandma and get together at your table. Eat, play, talk, laugh. Have some quality time with your loved ."
          }
        ]
    }



-- DOMAIN MODELS


type alias Pizza =
    { name : String
    , flour : Float
    , water : Float
    , yeast : Float
    , honey : Int
    , oliveoil : Float
    , salt : Float
    , steps : List PrepStep
    }


type alias PrepStep =
    { time : Int
    , title : String
    , description : String
    }
