module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Dom exposing (Error(..))
import Browser.Events
import Helper exposing (round2ToString, safeRegexOf)
import Html exposing (Html, button, div, img, input, label, span, text)
import Html.Attributes exposing (alt, attribute, class, classList, disabled, id, placeholder, src, style, type_)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import List
import Pizza.Model.Types exposing (..)
import Regex
import String
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( Front, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Model
    = Front
    | Carousel
    | RecipeAlbum (List Recipe) (Maybe String)
    | RecipeCalculator Recipe (Maybe Ingredient) Int (Maybe Float)



-- MSG


type Msg
    = GoCarousel
    | GoRecipeAlbum
    | GoRecipeCalculator Recipe
    | SelectIngredient Ingredient
    | UnselectIngredient
    | InputNewAmount String
    | InputSearchTerm String
    | CalculateRatio
    | Abort
    | Next
    | Prev
    | NoOp



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map
            (\key ->
                case key of
                    Enter ->
                        case model of
                            RecipeCalculator _ _ _ _ ->
                                CalculateRatio

                            _ ->
                                NoOp

                    _ ->
                        NoOp
            )
            (Browser.Events.onKeyDown keyDecoder)
        ]


keyDecoder : Decode.Decoder Key
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


type Key
    = Enter
    | Unknown


toKey : String -> Key
toKey str =
    case str of
        "Enter" ->
            Enter

        _ ->
            Unknown



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noChange =
            ( model, Cmd.none )
    in
    case msg of
        GoCarousel ->
            ( Carousel, Cmd.none )

        GoRecipeCalculator recipe ->
            ( RecipeCalculator
                recipe
                Nothing
                0
                Nothing
            , Cmd.none
            )

        GoRecipeAlbum ->
            ( RecipeAlbum
                [ samplePizzaRecipe
                , sampleLasagneRecipe
                ]
                Nothing
            , Cmd.none
            )

        SelectIngredient ingredient ->
            case model of
                RecipeCalculator recipe _ prepStepIndex maybeAmount ->
                    ( RecipeCalculator
                        recipe
                        (Just ingredient)
                        prepStepIndex
                        maybeAmount
                    , focus ingredient.id
                    )

                _ ->
                    noChange

        UnselectIngredient ->
            case model of
                RecipeCalculator recipe _ prepStepIndex _ ->
                    ( RecipeCalculator
                        recipe
                        Nothing
                        prepStepIndex
                        Nothing
                    , Cmd.none
                    )

                _ ->
                    noChange

        InputNewAmount amount ->
            case model of
                RecipeCalculator recipe maybeIngredient prepStepIndex _ ->
                    ( RecipeCalculator
                        recipe
                        maybeIngredient
                        prepStepIndex
                        (String.toFloat amount)
                    , Cmd.none
                    )

                _ ->
                    noChange

        InputSearchTerm searchTerm ->
            case model of
                RecipeAlbum recipes _ ->
                    ( RecipeAlbum
                        recipes
                        (Just searchTerm)
                    , Cmd.none
                    )

                _ ->
                    noChange

        CalculateRatio ->
            case model of
                RecipeCalculator recipe maybeIngredient prepStepIndex maybeNewAmount ->
                    ( Maybe.map2
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
                    , Cmd.none
                    )

                _ ->
                    noChange

        Abort ->
            case model of
                RecipeCalculator recipe _ prepStepIndex _ ->
                    ( RecipeCalculator
                        recipe
                        Nothing
                        prepStepIndex
                        Nothing
                    , Cmd.none
                    )

                _ ->
                    noChange

        Next ->
            case model of
                RecipeCalculator recipe maybeIngredient prepStepIndex maybeAmount ->
                    ( RecipeCalculator
                        recipe
                        maybeIngredient
                        (min
                            (prepStepIndex + 1)
                            (List.length recipe.steps)
                        )
                        maybeAmount
                    , Cmd.none
                    )

                _ ->
                    noChange

        Prev ->
            case model of
                RecipeCalculator recipe maybeIngredient prepStepIndex maybeAmount ->
                    ( RecipeCalculator
                        recipe
                        maybeIngredient
                        (max
                            (prepStepIndex - 1)
                            0
                        )
                        maybeAmount
                    , Cmd.none
                    )

                _ ->
                    noChange

        NoOp ->
            noChange



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Front ->
            frontView

        Carousel ->
            viewCarousel1

        RecipeAlbum recipes maybeSearchTerm ->
            recipeAlbumView
                (case maybeSearchTerm of
                    Just searchTerm ->
                        List.filter
                            (\recipe ->
                                List.any
                                    (String.contains searchTerm)
                                    [ recipe.label, recipe.id ]
                            )
                            recipes

                    Nothing ->
                        recipes
                )

        RecipeCalculator recipe selectedIngredient prepStepIndex maybeNewAmount ->
            recipeCalculatorView
                recipe
                selectedIngredient
                maybeNewAmount
                prepStepIndex


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
            [ onClick GoRecipeAlbum
            , class "btn btn-primary btn-lg"
            , style "margin-top" "2rem"
            , style "padding" "0.75rem 2rem"
            ]
            [ text "🍕 🧮" ]
        ]


carouselItems : List (Html msg)
carouselItems =
    [ carouselItem True "public/img/IMG_4365.jpeg"
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
        [ classList
            [ ( "carousel-item", True )
            , ( "active", isActive )
            ]
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


type ActiveTab
    = RecipeAlbumTab
    | RecipeCalculatorTab


navbarView : ActiveTab -> Html Msg
navbarView activeTab =
    let
        isRecipeAlbumActive =
            case activeTab of
                RecipeAlbumTab ->
                    True

                _ ->
                    False

        isRecipeCalculatorActive =
            case activeTab of
                RecipeCalculatorTab ->
                    True

                _ ->
                    False

        hasHistory =
            False

        navListItem : Bool -> String -> msg -> Html msg
        navListItem isActive label message =
            Html.li
                [ class "nav-item" ]
                [ Html.a
                    [ classList
                        [ ( "nav-link", True )
                        , ( "active", isActive )
                        ]
                    , if isActive then
                        attribute "aria-current" "page"

                      else
                        style "" ""
                    , Html.Attributes.href "#"
                    , onClick message
                    ]
                    [ text label ]
                ]
    in
    Html.nav
        [ class "navbar navbar-expand-lg bg-body-tertiary"
        ]
        [ div
            [ class "container-fluid" ]
            [ Html.a
                [ class "navbar-brand"
                , Html.Attributes.href "#"
                ]
                [ genericIcon "public/img/icon/pizza.svg" 32 ]
            , button
                [ class "navbar-toggler"
                , type_ "button"
                , attribute "data-bs-toggle" "collapse"
                , attribute "data-bs-target" "#navbarSupportedContent"
                , attribute "aria-controls" "navbarSupportedContent"
                , attribute "aria-expanded" "false"
                , attribute "aria-label" "Toggle navigation"
                ]
                [ span
                    [ class "navbar-toggler-icon"
                    ]
                    []
                ]
            , div
                [ class "collapse navbar-collapse"
                , id "navbarSupportedContent"
                ]
                [ Html.ul
                    [ class "navbar-nav me-auto mb-2 mb-lg-0"
                    ]
                    [ navListItem
                        isRecipeAlbumActive
                        "Recipes"
                        GoRecipeAlbum
                    , navListItem
                        isRecipeCalculatorActive
                        "Calculatore"
                        NoOp
                    , Html.li
                        [ class "nav-item dropdown"
                        ]
                        [ Html.a
                            [ classList
                                [ ( "nav-link", True )
                                , ( "dropdown-toggle", True )
                                , ( "disabled", not hasHistory )
                                ]
                            , Html.Attributes.href "#"
                            , attribute "role" "button"
                            , attribute "data-bs-toggle" "dropdown"
                            , attribute "aria-expanded" "false"
                            ]
                            [ text "History" ]
                        , Html.ul
                            [ class "dropdown-menu" ]
                            [ Html.li []
                                [ Html.a
                                    [ class "dropdown-item"
                                    , Html.Attributes.href "#"
                                    ]
                                    [ text "Action" ]
                                ]
                            , Html.li []
                                [ Html.a
                                    [ class "dropdown-item"
                                    , Html.Attributes.href "#"
                                    ]
                                    [ text "Another action" ]
                                ]
                            , Html.li
                                []
                                [ Html.hr
                                    [ class "dropdown-divider" ]
                                    []
                                ]
                            , Html.li
                                []
                                [ Html.a
                                    [ class "dropdown-item"
                                    , Html.Attributes.href "#"
                                    ]
                                    [ text "Something else here" ]
                                ]
                            ]
                        ]
                    ]
                , Html.form
                    [ class "d-flex"
                    , attribute "role" "search"
                    ]
                    [ input
                        [ classList
                            [ ( "form-control", True )
                            , ( "me-2", True )
                            , ( "disabled", not isRecipeAlbumActive )
                            ]
                        , type_ "search"
                        , placeholder "Search"
                        , attribute "aria-label" "Search"
                        , disabled (not isRecipeAlbumActive)
                        , onInput InputSearchTerm
                        ]
                        []
                    ]
                ]
            ]
        ]


recipeAlbumView : List Recipe -> Html Msg
recipeAlbumView recipes =
    div []
        [ navbarView RecipeAlbumTab
        , div
            [ class "album py-5" ]
            [ div
                [ class "container"
                , style "max-width" "700px"
                ]
                [ div
                    [ class "row row-cols-1 row-cols-sm-2 row-cols-md-3 g-3" ]
                    (List.map recipeAlbumCardView recipes)
                ]
            ]
        ]


recipeAlbumCardView : Recipe -> Html Msg
recipeAlbumCardView recipe =
    div
        [ class "col" ]
        [ div
            [ class "card shadow-sm h-100" ]
            [ Html.img
                [ class "card-img-top"
                , src
                    (case recipe.image of
                        Path path ->
                            path
                    )
                , alt recipe.label
                , style "object-fit" "cover"
                , style "height" "225px"
                ]
                []
            , div
                [ class "card-body d-flex flex-column" ]
                [ Html.h5
                    [ class "card-title" ]
                    [ text recipe.label ]
                , Html.p
                    [ class "card-text flex-grow-1" ]
                    [ text recipe.description ]
                , div
                    [ class "d-flex justify-content-between align-items-center mt-2" ]
                    [ div
                        [ class "btn-group" ]
                        [ if List.isEmpty recipe.ingredients && List.isEmpty recipe.steps then
                            button
                                [ class "btn btn-sm btn-outline-primary disabled"
                                ]
                                [ text "Under construction :(" ]

                          else
                            button
                                [ class "btn btn-sm btn-outline-primary"
                                , onClick (GoRecipeCalculator recipe)
                                ]
                                [ text "Open" ]
                        ]
                    ]
                ]
            ]
        ]


recipeCalculatorView : Recipe -> Maybe Ingredient -> Maybe Float -> Int -> Html Msg
recipeCalculatorView recipe selectedIngredient maybeNewAmount currentDisplayedPrepStepIndex =
    let
        tabListItem : String -> String -> String -> Bool -> Html msg
        tabListItem buttonId contentId label isActive =
            Html.li
                [ class "nav-item"
                , attribute "role" "presentation"
                ]
                [ button
                    [ classList
                        [ ( "nav-link", True )
                        , ( "active", isActive )
                        ]
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
                [ classList
                    [ ( "tab-pane", True )
                    , ( "show", isShow )
                    , ( "active", isActive )
                    ]
                , id contentId
                , attribute "role" "tabpanel"
                , attribute "aria-labelledby" tabLiId
                ]
                [ content
                ]
    in
    div []
        [ navbarView RecipeCalculatorTab
        , div
            [ class "mx-auto my-md-3 px-3 px-md-0"
            , style "max-width" "700px"
            ]
            [ div
                []
                [ Html.h1
                    []
                    [ text recipe.label ]
                , Html.ul
                    [ class "nav nav-tabs"
                    , id "recipeTabs"
                    , attribute "role" "tablist"
                    ]
                    [ tabListItem
                        "ingredients-tab"
                        "ingredients-content"
                        "Ingredients"
                        True
                    , tabListItem
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
        ]


focus : String -> Cmd Msg
focus id =
    Browser.Dom.focus id
        |> Task.attempt (\_ -> NoOp)


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
        [ class "mb-3"
        ]
        [ label
            []
            [ text ingredient.label ]
        , div
            [ class "input-group" ]
            [ input
                [ Html.Attributes.id ingredient.id
                , type_ "number"
                , Html.Attributes.autofocus isSelected
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
                inputButton (SelectIngredient ingredient) pencilIcon
            ]
        , if isSelected && not isNewAmountValid then
            div
                [ class "invalid-feedback" ]
                [ text "Amount must be ≥ 1" ]

          else
            text ""
        ]


prepStepsView : Int -> List Ingredient -> List PrepStep -> Html Msg
prepStepsView indexToDisplay ingredients prepSteps =
    let
        prepStepButton : Msg -> Bool -> String -> Html Msg
        prepStepButton message isDisabled label =
            button
                [ onClick message
                , disabled isDisabled
                , class "btn btn-primary btn-lg"
                , style "margin-top" "2rem"
                , style "padding" "0.75rem 2rem"
                ]
                [ text label ]
    in
    if List.length prepSteps == 0 then
        text "no steps :("

    else
        div
            [ style "display" "grid" ]
            [ div
                [ style "display" "grid"
                ]
                (List.indexedMap
                    (prepStepView indexToDisplay ingredients)
                    prepSteps
                )
            , div
                [ class "mb-3"
                , style "display" "grid"
                , style "grid-template-columns" "1fr 1fr"
                , style "gap" "0.75rem"
                ]
                [ prepStepButton
                    Prev
                    (indexToDisplay <= 0)
                    "←"
                , prepStepButton
                    Next
                    (indexToDisplay >= List.length prepSteps - 1)
                    "→"
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



-- SAMPLE DATA


sampleLasagneRecipe : Recipe
sampleLasagneRecipe =
    { id = "lasanche"
    , label = "Lasanche"
    , image = Path "public/img/lasanche.jpg"
    , description = ""
    , ingredients = []
    , steps = []
    }


samplePizzaRecipe : Recipe
samplePizzaRecipe =
    { id = "seven-hours-pizza-dough"
    , label = "Pizza dough (7 hours)"
    , image = Path "public/img/7-hours-pizza-dough.jpg"
    , description = ""
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
        fractionOfWordRegex : Regex.Regex
        fractionOfWordRegex =
            safeRegexOf "\\b\\d+/\\d+ of \\w+\\b"

        -- Find matches
        matches : List Regex.Match
        matches =
            Regex.find fractionOfWordRegex string

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
    List.foldl replaceMatch string matches



-- helper


pencilIcon : Html msg
pencilIcon =
    genericIcon "public/img/icon/pencil.svg" 16


checkIcon : Html msg
checkIcon =
    genericIcon "public/img/icon/check.svg" 16


closeIcon : Html msg
closeIcon =
    genericIcon "public/img/icon/close.svg" 16


genericIcon : String -> Int -> Html msg
genericIcon path width =
    Html.img
        [ Html.Attributes.width width
        , Html.Attributes.src path
        ]
        []
