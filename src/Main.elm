module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Dom exposing (Error(..))
import Helper exposing (round2ToString, safeRegexOf)
import Html exposing (Html, button, div, img, input, label, span, text)
import Html.Attributes exposing (alt, attribute, class, classList, disabled, id, placeholder, src, style, type_)
import Html.Events exposing (onClick, onInput)
import List
import Regex
import String



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
    | RecipeCalculator Recipe (Maybe Ingredient) Int (Maybe Float)



-- MSG


type Msg
    = GoCarousel
    | GoRecipeCalculator Recipe
    | SelectIngredient (Maybe Ingredient)
    | InputNewAmount String
    | CalculateRatio
    | Abort
    | Next
    | Prev
    | NoOp



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        GoCarousel ->
            Carousel

        GoRecipeCalculator recipe ->
            RecipeCalculator recipe Nothing 0 Nothing

        SelectIngredient maybeIngredient ->
            case model of
                RecipeCalculator recipe _ prepStepIndex maybeAmount ->
                    RecipeCalculator recipe maybeIngredient prepStepIndex maybeAmount

                _ ->
                    model

        InputNewAmount amount ->
            case model of
                RecipeCalculator recipe maybeIngredient prepStepIndex _ ->
                    RecipeCalculator
                        recipe
                        maybeIngredient
                        prepStepIndex
                        (String.toFloat amount)

                _ ->
                    model

        CalculateRatio ->
            case model of
                RecipeCalculator recipe maybeIngredient prepStepIndex maybeNewAmount ->
                    Maybe.map2
                        (\ingredient newAmount ->
                            let
                                oldAmount =
                                    ingredient.amount

                                newRatio =
                                    if oldAmount <= 0 then
                                        1

                                    else
                                        newAmount / oldAmount
                            in
                            RecipeCalculator
                                (recipeApplyRatio newRatio recipe)
                                Nothing
                                prepStepIndex
                                Nothing
                        )
                        maybeIngredient
                        maybeNewAmount
                        |> Maybe.withDefault
                            (RecipeCalculator
                                recipe
                                maybeIngredient
                                prepStepIndex
                                maybeNewAmount
                            )

                _ ->
                    model

        Abort ->
            case model of
                RecipeCalculator recipe _ prepStepIndex _ ->
                    RecipeCalculator
                        recipe
                        Nothing
                        prepStepIndex
                        Nothing

                _ ->
                    model

        Next ->
            case model of
                RecipeCalculator recipe maybeIngredient prepStepIndex maybeAmount ->
                    RecipeCalculator
                        recipe
                        maybeIngredient
                        (min
                            (prepStepIndex + 1)
                            (List.length recipe.steps)
                        )
                        maybeAmount

                _ ->
                    model

        Prev ->
            case model of
                RecipeCalculator recipe maybeIngredient prepStepIndex maybeAmount ->
                    RecipeCalculator
                        recipe
                        maybeIngredient
                        (max
                            (prepStepIndex - 1)
                            0
                        )
                        maybeAmount

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

        RecipeCalculator recipe selectedIngredient prepStepIndex maybeNewAmount ->
            recipeView
                recipe
                selectedIngredient
                maybeNewAmount
                prepStepIndex



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
            [ onClick (GoRecipeCalculator samplePizzaRecipe)
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


recipeView : Recipe -> Maybe Ingredient -> Maybe Float -> Int -> Html Msg
recipeView recipe selectedIngredient maybeNewAmount currentDisplayedPrepStepIndex =
    let
        tabLi buttonId contentId label isActive =
            Html.li
                [ class "nav-item"
                , attribute "role" "presentation"
                ]
                [ button
                    [ class
                        ("nav-link"
                            ++ (if isActive then
                                    " active"

                                else
                                    ""
                               )
                        )
                    , id buttonId
                    , attribute "data-bs-toggle" "tab"
                    , attribute "data-bs-target" ("#" ++ contentId)
                    , attribute "type" "button"
                    , attribute "role" "tab"
                    , attribute "aria-controls" contentId
                    , attribute "aria-selected" "true"
                    ]
                    [ text label ]
                ]

        tabContent contentId tabLiId content isShow isActive =
            div
                [ class
                    ("tab-pane"
                        ++ (if isShow then
                                " show"

                            else
                                ""
                           )
                        ++ (if isActive then
                                " active"

                            else
                                ""
                           )
                    )
                , id contentId
                , attribute "role" "tabpanel"
                , attribute "aria-labelledby" tabLiId
                ]
                [ content
                ]
    in
    div
        [ class "card mx-auto my-md-3"
        , style "max-width" "700px"
        ]
        [ div
            [ class "card-body" ]
            [ Html.h2
                [ class "card-title" ]
                [ text recipe.label ]
            , Html.ul
                [ class "nav nav-tabs"
                , id "recipeTabs"
                , attribute "role" "tablist"
                ]
                [ tabLi
                    "ingredients-tab"
                    "ingredients-content"
                    "Ingredients"
                    True
                , tabLi
                    "prepSteps-tab"
                    "prepSteps-content"
                    "Steps"
                    False
                ]
            , div
                [ class "tab-content"
                , id "recipeTabsContent"
                ]
                [ tabContent
                    "ingredients-content"
                    "ingredients-tab"
                    (ingredientsView
                        recipe.ingredients
                        selectedIngredient
                        maybeNewAmount
                    )
                    True
                    True
                , tabContent
                    "prepSteps-content"
                    "prepSteps-tab"
                    (prepStepsView
                        currentDisplayedPrepStepIndex
                        recipe.ingredients
                        recipe.steps
                    )
                    False
                    False
                ]
            ]
        ]


ingredientsView : List Ingredient -> Maybe Ingredient -> Maybe Float -> Html Msg
ingredientsView ingredients selectedIngredient maybeNewAmount =
    div
        []
        (List.map (ingredientView selectedIngredient maybeNewAmount) ingredients)


ingredientView : Maybe Ingredient -> Maybe Float -> Ingredient -> Html Msg
ingredientView maybeSelectedIngredient maybeNewAmount ingredient =
    let
        isSelected =
            maybeSelectedIngredient
                |> Maybe.map (\selected -> selected.id == ingredient.id)
                |> Maybe.withDefault False

        isNewAmountValid =
            maybeNewAmount
                |> Maybe.map (\newAmount -> newAmount >= 1)
                |> Maybe.withDefault False

        inputButton : Msg -> Html Msg -> Html Msg
        inputButton onClickMessage icon =
            button
                [ type_ "button"
                , class "btn btn-outline-secondary"
                , style "border-left" "none"
                , style "border-radius" "0 .25rem .25rem 0"
                , style "padding" "0 0.75rem"
                , onClick onClickMessage
                ]
                [ icon ]
    in
    div
        [ class "mb-3" ]
        [ label [] [ text ingredient.label ]
        , div [ class "input-group" ]
            [ input
                [ Html.Attributes.id ingredient.id
                , type_ "number"
                , classList
                    [ ( "form-control", True )
                    , ( "is-invalid", isSelected && not isNewAmountValid )
                    ]
                , placeholder
                    (round2ToString ingredient.amount
                        ++ " "
                        ++ unitToAbbr ingredient.unit
                    )
                , disabled (not isSelected)
                , if isSelected then
                    style "" ""

                  else
                    Html.Attributes.value ""
                , onInput InputNewAmount
                ]
                []
            , if isSelected then
                if isNewAmountValid then
                    inputButton CalculateRatio checkIcon

                else
                    inputButton Abort closeIcon

              else
                inputButton (SelectIngredient (Just ingredient)) pencilIcon
            ]
        , if isSelected && not isNewAmountValid then
            div [ class "invalid-feedback" ] [ text "Amount must be ≥ 1" ]

          else
            text ""
        ]


prepStepsView : Int -> List Ingredient -> List PrepStep -> Html Msg
prepStepsView indexToDisplay ingredients prepSteps =
    if List.length prepSteps == 0 then
        text "no steps :("

    else
        div
            [ style "display" "grid" ]
            [ div
                [ style "display" "grid"
                ]
                (List.indexedMap (prepStepView indexToDisplay ingredients) prepSteps)
            , div
                [ class "mb-3"
                , style "display" "grid"
                , style "grid-template-columns" "1fr 1fr"
                , style "gap" "0.75rem"
                ]
                [ button
                    [ onClick Prev
                    , disabled (indexToDisplay <= 0)
                    , class "btn btn-primary btn-lg"
                    , style "margin-top" "2rem"
                    , style "padding" "0.75rem 2rem"
                    ]
                    [ text "←" ]
                , button
                    [ onClick Next
                    , disabled (indexToDisplay >= List.length prepSteps - 1)
                    , class "btn btn-primary btn-lg"
                    , style "margin-top" "2rem"
                    , style "padding" "0.75rem 2rem"
                    ]
                    [ text "→" ]
                ]
            ]



{- All elements are rendered but displayed conditionally by visibility. By this the layout is fixed on the start and does not crash -}


prepStepView : Int -> List Ingredient -> Int -> PrepStep -> Html Msg
prepStepView indexToDisplay ingredients index prepStep =
    div
        [ style "grid-row" "1"
        , style "grid-column" "1"
        , style "margin-top" "1rem"
        , if indexToDisplay == index then
            style "visibility" "visible"

          else
            style "visibility" "hidden"
        , style "transition" "opacity 1000ms ease"
        , if indexToDisplay == index then
            style "opacity" "1"

          else
            style "opacity" "0"
        ]
        [ Html.h3
            []
            [ text (String.fromInt (index + 1) ++ ". " ++ prepStep.title) ]
        , div []
            [ if prepStep.time == -1 then
                text "∞"

              else if prepStep.time == 0 then
                text ""

              else
                text (String.fromInt prepStep.time ++ " mins")
            ]
        , div
            []
            [ text
                (replaceIngredientAmountFraction
                    ingredients
                    prepStep.description
                )
            ]
        ]


type Unit
    = Gram
    | Mililiter
    | Teaspoon


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


samplePizzaRecipe : Recipe
samplePizzaRecipe =
    { id = "seven-hours-pizza-dough"
    , label = "Seven hours pizza dough"
    , ingredients =
        [ { id = "flour"
          , label = "Flour"
          , amount = 496
          , unit = Gram
          }
        , { id = "water"
          , label = "Water"
          , amount = 313
          , unit = Gram
          }
        , { id = "yeast"
          , label = "Yeast"
          , amount = 3.4
          , unit = Gram
          }
        , { id = "oliveoil"
          , label = "Olive oil"
          , amount = 12
          , unit = Mililiter
          }
        , { id = "salt"
          , label = "Salt"
          , amount = 15
          , unit = Gram
          }
        , { id = "honey"
          , label = "Honey"
          , amount = 1
          , unit = Teaspoon
          }
        ]
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
          , description = "Put all ingredients to flour/water bowl and knead, as if your life depends on it. The dough is ready, when it stops being clingy"
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
          , title = "What belongs together will be together in the end"
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
          , description = "You need instructions for that too?"
          }
        , { time = -1
          , title = "I knew it"
          , description = "Call some friends, your parents, grandma and get together at your table. Eat, play, talk, laugh. Have some quality time with your loved ones."
          }
        ]
    }


replaceIngredientAmountFraction : List Ingredient -> String -> String
replaceIngredientAmountFraction ingredients string =
    let
        -- Regex for "fraction of word"
        fractionOfWordRegex : Regex.Regex
        fractionOfWordRegex =
            safeRegexOf "\\b\\d+/\\d+ of \\w+\\b"

        -- Find matches
        matches : List Regex.Match
        matches =
            Regex.find fractionOfWordRegex string

        -- Parse fraction string like "4/5" -> 0.8
        parseFraction : String -> Maybe Float
        parseFraction str =
            case String.split "/" str of
                [ numStr, denomStr ] ->
                    case ( String.toFloat numStr, String.toFloat denomStr ) of
                        ( Just n, Just d ) ->
                            Just (n / d)

                        _ ->
                            Nothing

                _ ->
                    Nothing

        -- Replace one match in the string
        replaceMatch : Regex.Match -> String -> String
        replaceMatch match str =
            let
                fullMatch =
                    match.match

                parts =
                    String.words fullMatch

                -- ["4/5", "of", "sugar"]
                maybeFraction : Maybe Float
                maybeFraction =
                    case parts of
                        frac :: "of" :: _ :: [] ->
                            parseFraction frac

                        _ ->
                            Nothing

                maybeIngredient : Maybe Ingredient
                maybeIngredient =
                    case parts of
                        _ :: _ :: word :: [] ->
                            case
                                List.filter
                                    (\ingredient ->
                                        ingredient.id == word || ingredient.label == word
                                    )
                                    ingredients
                            of
                                ingredient :: [] ->
                                    Just ingredient

                                _ ->
                                    Nothing

                        _ ->
                            Nothing
            in
            case ( maybeFraction, maybeIngredient ) of
                ( Just f, Just ing ) ->
                    let
                        newAmount =
                            f * ing.amount
                    in
                    String.replace
                        fullMatch
                        (round2ToString newAmount
                            ++ unitToAbbr ing.unit
                            ++ " "
                            ++ ing.id
                        )
                        str

                _ ->
                    str
    in
    -- Apply replacements for all matches
    List.foldl replaceMatch string matches



-- DOMAIN MODELS


type alias Recipe =
    { id : String
    , label : String
    , ingredients : List Ingredient
    , steps : List PrepStep
    }


type alias Ingredient =
    { id : String
    , label : String
    , amount : Float
    , unit : Unit
    }


recipeApplyRatio : Float -> Recipe -> Recipe
recipeApplyRatio ratio recipe =
    { recipe
        | ingredients =
            List.map (ingredientApplyRatio ratio) recipe.ingredients
    }


ingredientApplyRatio : Float -> Ingredient -> Ingredient
ingredientApplyRatio ratio ingredient =
    { ingredient
        | amount = ingredient.amount * ratio
    }


type alias PrepStep =
    { time : Int
    , title : String
    , description : String
    }



-- helper


pencilIcon : Html msg
pencilIcon =
    Html.img
        [ Html.Attributes.width 16
        , Html.Attributes.src "src/img/icon/pencil.svg"
        ]
        []


checkIcon : Html msg
checkIcon =
    Html.img
        [ Html.Attributes.width 16
        , Html.Attributes.src "src/img/icon/check.svg"
        ]
        []


closeIcon : Html msg
closeIcon =
    Html.img
        [ Html.Attributes.width 16
        , Html.Attributes.src "src/img/icon/close.svg"
        ]
        []
