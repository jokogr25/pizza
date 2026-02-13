module Page.Recipe.View exposing (..)

import Domain.ActionButton as ActionButton
import Domain.Helper exposing (round2ToString)
import Domain.Icon exposing (ionIcon)
import Domain.Recipe exposing (Ingredient, Path(..), PrepStep, Recipe, Unit(..), replaceIngredientAmountFraction, unitToAbbr)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, disabled, id, style, type_)
import Html.Events exposing (onClick, onInput)
import Maybe


type Tab
    = Ingredients
    | Steps


type Model
    = View (List Recipe) Recipe (Maybe Ingredient) Int (Maybe Float) Float Tab


type Msg
    = Refresh
    | SelectIngredient Ingredient
    | UnselectIngredient
    | SelectTab Tab
    | InputNewAmount String
    | CalculateRatio
    | Abort
    | Next
    | Prev
    | NoOp


init : List Recipe -> Recipe -> Model
init recipes recipe =
    View recipes recipe Nothing 0 Nothing 1 Ingredients


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectTab page ->
            case model of
                View recipes recipe maybeIngredient prepStepIndex maybeAmount ratio _ ->
                    View
                        recipes
                        recipe
                        maybeIngredient
                        prepStepIndex
                        maybeAmount
                        ratio
                        page

        SelectIngredient ingredient ->
            case model of
                View recipes recipe _ prepStepIndex _ ratio page ->
                    View
                        recipes
                        recipe
                        (Just ingredient)
                        prepStepIndex
                        Nothing
                        ratio
                        page

        --focus ingredient.id
        UnselectIngredient ->
            case model of
                View recipes recipe _ prepStepIndex _ ratio page ->
                    View
                        recipes
                        recipe
                        Nothing
                        prepStepIndex
                        Nothing
                        ratio
                        page

        Refresh ->
            case model of
                View recipes recipe _ _ _ _ page ->
                    View
                        recipes
                        recipe
                        Nothing
                        0
                        Nothing
                        1
                        page

        InputNewAmount amount ->
            case model of
                View recipes recipe maybeIngredient prepStepIndex _ ratio page ->
                    View
                        recipes
                        recipe
                        maybeIngredient
                        prepStepIndex
                        (String.toFloat amount)
                        ratio
                        page

        CalculateRatio ->
            case model of
                View recipes recipe maybeIngredient prepStepIndex maybeNewAmount _ page ->
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
                            View
                                recipes
                                recipe
                                Nothing
                                prepStepIndex
                                Nothing
                                newRatio
                                page
                        )
                        maybeIngredient
                        maybeNewAmount
                        |> Maybe.withDefault
                            (View
                                recipes
                                recipe
                                maybeIngredient
                                prepStepIndex
                                maybeNewAmount
                                1
                                page
                            )

        Abort ->
            case model of
                View recipes recipe _ prepStepIndex _ ratio page ->
                    View
                        recipes
                        recipe
                        Nothing
                        prepStepIndex
                        Nothing
                        ratio
                        page

        Next ->
            case model of
                View recipes recipe maybeIngredient prepStepIndex maybeAmount ratio page ->
                    View
                        recipes
                        recipe
                        maybeIngredient
                        (min
                            (prepStepIndex + 1)
                            (List.length recipe.steps - 1)
                        )
                        maybeAmount
                        ratio
                        page

        Prev ->
            case model of
                View recipes recipe maybeIngredient prepStepIndex maybeAmount ratio page ->
                    View
                        recipes
                        recipe
                        maybeIngredient
                        (max
                            (prepStepIndex - 1)
                            0
                        )
                        maybeAmount
                        ratio
                        page

        NoOp ->
            model


view : Model -> Html Msg
view model =
    case model of
        View _ recipe selectedIngredient prepStepIndex maybeNewAmount ratio page ->
            div
                [ class "d-flex flex-column h-100"
                ]
                [ div
                    [ class "flex-grow-1 overflow-auto"
                    ]
                    [ recipeView
                        recipe
                        ratio
                        selectedIngredient
                        maybeNewAmount
                        prepStepIndex
                        page
                    ]
                , footerView model
                ]


recipeView : Recipe -> Float -> Maybe Ingredient -> Maybe Float -> Int -> Tab -> Html Msg
recipeView recipe ratio selectedIngredient maybeNewAmount currentDisplayedPrepStepIndex activePage =
    let
        tabListItem : String -> String -> String -> Bool -> Tab -> Html Msg
        tabListItem buttonId contentId label isActive pageToActivate =
            Html.li
                [ class "nav-item flex-fill"
                , attribute "role" "presentation"
                ]
                [ button
                    [ classList
                        [ ( "nav-link", True )
                        , ( "w-100", True )
                        , ( "justify-content-center", True )
                        , ( "text-center", True )
                        , ( "text-muted", not isActive )
                        , ( "active", isActive )
                        ]
                    , id buttonId
                    , onClick (SelectTab pageToActivate)
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
    div
        [ style "max-width" "700px"
        , class "mx-auto my-3 my-md-4"
        ]
        [ div
            []
            [ Html.h1
                []
                [ text recipe.label ]
            ]
        , div
            [ class "border rounded" ]
            [ Html.ul
                [ class "nav nav-underline border-bottom"
                , id "recipeTabs"
                , attribute "role" "tablist"
                ]
                [ tabListItem
                    "ingredients-tab"
                    "ingredients-content"
                    "Ingredients"
                    (case activePage of
                        Ingredients ->
                            True

                        _ ->
                            False
                    )
                    Ingredients
                , tabListItem
                    "prepSteps-tab"
                    "prepSteps-content"
                    "Steps"
                    (case activePage of
                        Steps ->
                            True

                        _ ->
                            False
                    )
                    Steps
                ]
            , div
                [ class "tab-content p-3"
                , style "overflow-y" "auto"
                , style "height" "60vh"
                , id "recipeTabsContent"
                ]
                [ tabContent
                    "ingredients-content"
                    "ingredients-tab"
                    (ingredientsView
                        recipe.ingredients
                        ratio
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


footerView : Model -> Html Msg
footerView model =
    case model of
        View _ recipe _ prepStepIndex _ ratio activeTab ->
            let
                isSteps =
                    activeTab == Steps

                isFirst =
                    prepStepIndex == 0

                isLast =
                    prepStepIndex == List.length recipe.steps - 1
            in
            div
                [ class "p-3 border-top"
                ]
                [ div
                    [ class "container d-flex justify-content-center"
                    ]
                    (if isSteps then
                        List.map ActionButton.actionToIcon
                            [ ActionButton.Pre Prev isFirst
                            , ActionButton.Nex Next isLast
                            ]
                        -- ]

                     else
                        List.map ActionButton.actionToIcon
                            [ ActionButton.Refresh
                                Refresh
                                (ratio == 1)
                            ]
                    )
                ]


ingredientsView : List Ingredient -> Float -> Maybe Ingredient -> Maybe Float -> Html Msg
ingredientsView ingredients ratio selectedIngredient maybeNewAmount =
    div []
        [ div
            []
            (List.map
                (ingredientView ratio selectedIngredient maybeNewAmount)
                ingredients
            )
        ]


ingredientView : Float -> Maybe Ingredient -> Maybe Float -> Ingredient -> Html Msg
ingredientView ratio maybeSelectedIngredient maybeNewAmount ingredient =
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
                [ icon
                ]

        placeholder =
            ingredient.label
                ++ " ("
                ++ round2ToString (ingredient.amount * ratio)
                ++ " "
                ++ unitToAbbr ingredient.unit
                ++ ")"

        purgeValue =
            if isSelected then
                Html.Attributes.style "" ""

            else
                Html.Attributes.value ""

        btn =
            let
                size =
                    20
            in
            if isSelected then
                if isNewAmountValid then
                    inputButton CalculateRatio (ionIcon "checkmark" size)

                else
                    inputButton Abort (ionIcon "close" size)

            else
                inputButton (SelectIngredient ingredient) (ionIcon "pencil" size)
    in
    div
        [ class "mt-3"
        ]
        [ div
            [ class "input-group mb-3"
            ]
            [ input
                [ Html.Attributes.id ingredient.id
                , Html.Attributes.autofocus isSelected
                , Html.Attributes.placeholder placeholder
                , style "font-size" "1.1rem"
                , style "padding" "0.5rem 0.75rem"
                , style "height" "calc(2.5rem + 2px)"
                , type_ "number"
                , classList
                    [ ( "form-control", True )
                    , ( "is-invalid", isSelected && not isNewAmountValid )
                    ]
                , disabled (not isSelected)
                , purgeValue
                , onInput InputNewAmount
                ]
                []
            , btn
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
    if List.length prepSteps == 0 then
        text "no steps :("

    else
        div
            [ style "display" "grid"
            ]
            [ div
                [ style "display" "grid"
                ]
                (List.indexedMap
                    (prepStepView indexToDisplay ingredients)
                    prepSteps
                )
            ]


prepStepView : Int -> List Ingredient -> Int -> PrepStep -> Html Msg
prepStepView indexToDisplay ingredients index prepStep =
    let
        visibility =
            if indexToDisplay == index then
                "visible"

            else
                "hidden"

        opacity =
            if indexToDisplay == index then
                "1"

            else
                "0"

        title =
            String.fromInt (index + 1) ++ ". " ++ prepStep.title

        time =
            if prepStep.time == -1 then
                "∞"

            else if prepStep.time == 0 then
                ""

            else
                String.fromInt prepStep.time ++ " mins"

        description =
            replaceIngredientAmountFraction
                ingredients
                prepStep.description
    in
    div
        [ style "grid-row" "1"
        , style "grid-column" "1"
        , style "margin-top" "1rem"
        , style "transition" "opacity 1000ms ease"
        , style "visibility" visibility
        , style "opacity" opacity
        ]
        [ Html.h3
            []
            [ text title ]
        , div
            []
            [ text time
            ]
        , div
            []
            [ text description
            ]
        ]
