module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Dom exposing (Error(..))
import Browser.Events
import Domain.Helper exposing (..)
import Domain.Icon exposing (..)
import Domain.Recipe exposing (..)
import Html exposing (Html, button, div, img, input, label, span, text)
import Html.Attributes exposing (alt, attribute, class, classList, disabled, id, src, style, type_)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import List
import Page.Recipe.Album as RecipeAlbum exposing (Model(..))
import Page.Recipe.Create as RecipeCreate exposing (Model(..))
import Platform.Cmd as Cmd
import Regex
import String
import Svg.Attributes exposing (visibility)
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( Front Nothing, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Model
    = Front (Maybe (List Recipe))
    | Carousel
    | RecipeAlbum RecipeAlbum.Model
    | RecipeCreate RecipeCreate.Model
    | RecipeViewer (List Recipe) Recipe (Maybe Ingredient) Int (Maybe Float) Float ActivePage



-- MSG


type Msg
    = GoFront
    | GoCarousel
    | GoRecipeAlbum
    | GoRecipeViewer Recipe ActivePage
    | ResetRecipeViewer
    | SelectIngredient Ingredient
    | SelectPage ActivePage
    | UnselectIngredient
    | InputNewAmount String
    | InputSearchTerm String
    | CalculateRatio
    | Abort
    | Next
    | Prev
    | NoOp
      -- SubComponents
    | AlbumMsg RecipeAlbum.Msg
    | CreateMsg RecipeCreate.Msg



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map
            (\key ->
                case key of
                    Enter ->
                        case model of
                            RecipeViewer _ _ (Just _) _ (Just newAmount) _ _ ->
                                if newAmount >= 1 then
                                    CalculateRatio

                                else
                                    NoOp

                            _ ->
                                NoOp

                    Left ->
                        case model of
                            RecipeViewer _ _ _ _ _ _ RecipeStepsPage ->
                                Prev

                            _ ->
                                NoOp

                    Right ->
                        case model of
                            RecipeViewer _ _ _ _ _ _ RecipeStepsPage ->
                                Next

                            _ ->
                                NoOp

                    Unknown ->
                        NoOp
            )
            (Browser.Events.onKeyDown keyDecoder)
        ]


keyDecoder : Decode.Decoder Key
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


type Key
    = Enter
    | Left
    | Right
    | Unknown


toKey : String -> Key
toKey str =
    case str of
        "Enter" ->
            Enter

        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

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
        GoFront ->
            case model of
                RecipeAlbum (RecipeAlbum.Album recipes _) ->
                    ( Front (Just recipes)
                    , Cmd.none
                    )

                RecipeViewer recipes _ _ _ _ _ _ ->
                    ( Front (Just recipes)
                    , Cmd.none
                    )

                RecipeCreate (RecipeCreate.Create recipes _ _ _) ->
                    ( Front (Just recipes)
                    , Cmd.none
                    )

                _ ->
                    noChange

        GoCarousel ->
            ( Carousel, Cmd.none )

        GoRecipeViewer recipe page ->
            case model of
                RecipeAlbum (RecipeAlbum.Album recipes _) ->
                    ( RecipeViewer
                        recipes
                        recipe
                        Nothing
                        0
                        Nothing
                        1
                        page
                    , Cmd.none
                    )

                _ ->
                    noChange

        GoRecipeAlbum ->
            case model of
                Front maybeRecipes ->
                    let
                        recipes =
                            maybeRecipes
                                |> Maybe.withDefault
                                    [ samplePizzaRecipe
                                    , sampleLasagneRecipe
                                    ]
                    in
                    ( RecipeAlbum
                        (RecipeAlbum.Album
                            recipes
                            Nothing
                        )
                    , Cmd.none
                    )

                RecipeViewer recipes _ _ _ _ _ _ ->
                    ( RecipeAlbum
                        (RecipeAlbum.update
                            RecipeAlbum.NoOp
                            (RecipeAlbum.Album
                                recipes
                                Nothing
                            )
                        )
                    , Cmd.none
                    )

                _ ->
                    noChange

        SelectPage page ->
            case model of
                RecipeViewer recipes recipe maybeIngredient prepStepIndex maybeAmount ratio _ ->
                    ( RecipeViewer
                        recipes
                        recipe
                        maybeIngredient
                        prepStepIndex
                        maybeAmount
                        ratio
                        page
                    , Cmd.none
                    )

                _ ->
                    noChange

        SelectIngredient ingredient ->
            case model of
                RecipeViewer recipes recipe _ prepStepIndex _ ratio page ->
                    ( RecipeViewer
                        recipes
                        recipe
                        (Just ingredient)
                        prepStepIndex
                        Nothing
                        ratio
                        page
                    , focus ingredient.id
                    )

                _ ->
                    noChange

        UnselectIngredient ->
            case model of
                RecipeViewer recipes recipe _ prepStepIndex _ ratio page ->
                    ( RecipeViewer
                        recipes
                        recipe
                        Nothing
                        prepStepIndex
                        Nothing
                        ratio
                        page
                    , Cmd.none
                    )

                _ ->
                    noChange

        ResetRecipeViewer ->
            case model of
                RecipeViewer recipes recipe _ _ _ _ page ->
                    ( RecipeViewer
                        recipes
                        recipe
                        Nothing
                        0
                        Nothing
                        1
                        page
                    , Cmd.none
                    )

                _ ->
                    noChange

        InputNewAmount amount ->
            case model of
                RecipeViewer recipes recipe maybeIngredient prepStepIndex _ ratio page ->
                    ( RecipeViewer
                        recipes
                        recipe
                        maybeIngredient
                        prepStepIndex
                        (String.toFloat amount)
                        ratio
                        page
                    , Cmd.none
                    )

                _ ->
                    noChange

        InputSearchTerm searchTerm ->
            case model of
                RecipeAlbum (RecipeAlbum.Album recipes _) ->
                    ( RecipeAlbum
                        (RecipeAlbum.Album
                            recipes
                            (Just searchTerm)
                        )
                    , Cmd.none
                    )

                _ ->
                    noChange

        CalculateRatio ->
            case model of
                RecipeViewer recipes recipe maybeIngredient prepStepIndex maybeNewAmount _ page ->
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
                            RecipeViewer
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
                            (RecipeViewer
                                recipes
                                recipe
                                maybeIngredient
                                prepStepIndex
                                maybeNewAmount
                                1
                                page
                            )
                    , Cmd.none
                    )

                _ ->
                    noChange

        Abort ->
            case model of
                RecipeViewer recipes recipe _ prepStepIndex _ ratio page ->
                    ( RecipeViewer
                        recipes
                        recipe
                        Nothing
                        prepStepIndex
                        Nothing
                        ratio
                        page
                    , Cmd.none
                    )

                _ ->
                    noChange

        Next ->
            case model of
                RecipeViewer recipes recipe maybeIngredient prepStepIndex maybeAmount ratio page ->
                    ( RecipeViewer
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
                    , Cmd.none
                    )

                _ ->
                    noChange

        Prev ->
            case model of
                RecipeViewer recipes recipe maybeIngredient prepStepIndex maybeAmount ratio page ->
                    ( RecipeViewer
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
                    , Cmd.none
                    )

                _ ->
                    noChange

        AlbumMsg albumMsg ->
            case model of
                RecipeAlbum (RecipeAlbum.Album recipes _) ->
                    case albumMsg of
                        RecipeAlbum.Out outMsg ->
                            case outMsg of
                                RecipeAlbum.GoRecipeCreator ->
                                    ( RecipeCreate (RecipeCreate.init recipes)
                                    , Cmd.none
                                    )

                                RecipeAlbum.GoRecipeViewer recipe ->
                                    ( RecipeViewer
                                        recipes
                                        recipe
                                        Nothing
                                        0
                                        Nothing
                                        1
                                        RecipeIngredientsPage
                                    , Cmd.none
                                    )

                        _ ->
                            case model of
                                RecipeAlbum m ->
                                    ( RecipeAlbum
                                        (RecipeAlbum.update albumMsg m)
                                    , Cmd.none
                                    )

                                _ ->
                                    noChange

                _ ->
                    noChange

        CreateMsg createMsg ->
            case model of
                RecipeCreate (RecipeCreate.Create recipes draft maybeIngredient maybeStep) ->
                    case createMsg of
                        RecipeCreate.Out outMsg ->
                            case outMsg of
                                RecipeCreate.SaveRecipe recipe ->
                                    ( RecipeAlbum
                                        (RecipeAlbum.init (recipe :: recipes))
                                    , Cmd.none
                                    )

                        _ ->
                            ( RecipeCreate
                                (Tuple.first
                                    (RecipeCreate.update
                                        createMsg
                                        (RecipeCreate.Create recipes draft maybeIngredient maybeStep)
                                    )
                                )
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
        Front _ ->
            frontView

        Carousel ->
            viewCarousel1

        RecipeViewer _ recipe selectedIngredient prepStepIndex maybeNewAmount ratio page ->
            contentView
                page
                (recipeView
                    recipe
                    ratio
                    selectedIngredient
                    maybeNewAmount
                    prepStepIndex
                    page
                )
                (case page of
                    RecipeIngredientsPage ->
                        Just (ingredientsViewActions ratio)

                    RecipeStepsPage ->
                        Just
                            (prepStepsViewActions
                                prepStepIndex
                                (List.length recipe.steps)
                            )

                    _ ->
                        Nothing
                )

        RecipeAlbum m ->
            contentView
                RecipeAlbumPage
                (RecipeAlbum.view m
                    |> Html.map AlbumMsg
                )
                Nothing

        RecipeCreate m ->
            contentView
                RecipeCreatorPage
                (RecipeCreate.view m
                    |> Html.map CreateMsg
                )
                Nothing


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
            [ ionIcon "pizza" 32
            ]
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


type ActivePage
    = RecipeAlbumPage
    | RecipeIngredientsPage
    | RecipeStepsPage
    | RecipeCreatorPage


contentView : ActivePage -> Html Msg -> Maybe (Html Msg) -> Html Msg
contentView activePage content maybeActions =
    div
        [ class "d-flex flex-column vh-100"
        ]
        [ navbarView activePage
        , div
            [ class "flex-grow-1 overflow-auto"
            ]
            [ content
            ]
        , maybeActions
            |> Maybe.map footerView
            |> Maybe.withDefault (text "")
        ]


footerView : Html Msg -> Html Msg
footerView actions =
    div
        [ class "p-3 border-top"
        ]
        [ div
            [ class "w-100" ]
            [ actions ]
        ]


navbarView : ActivePage -> Html Msg
navbarView activePage =
    let
        isRecipeAlbumActive =
            case activePage of
                RecipeAlbumPage ->
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
                    , onClick message
                    ]
                    [ text label ]
                ]
    in
    Html.nav
        [ class "navbar navbar-expand-sm bg-body-tertiary border-bottom"
        ]
        [ div
            [ class "container-fluid" ]
            [ Html.a
                [ class "navbar-brand"
                , onClick GoFront
                ]
                [ ionIcon "pizza" 32 ]
            , button
                [ class "navbar-toggler collapsed"
                , type_ "button"
                , attribute "data-bs-toggle" "collapse"
                , attribute "data-bs-target" "#navbarSupportedContent"
                , attribute "aria-controls" "navbarSupportedContent"
                , attribute "aria-label" "Toggle navigation"
                ]
                [ ionIcon "menu" 32
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
                        , Html.Attributes.placeholder "Search"
                        , attribute "aria-label" "Search"
                        , disabled (not isRecipeAlbumActive)
                        , onInput InputSearchTerm
                        ]
                        []
                    ]
                ]
            ]
        ]


recipeView : Recipe -> Float -> Maybe Ingredient -> Maybe Float -> Int -> ActivePage -> Html Msg
recipeView recipe ratio selectedIngredient maybeNewAmount currentDisplayedPrepStepIndex activePage =
    let
        tabListItem : String -> String -> String -> Bool -> ActivePage -> Html Msg
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
                    , onClick (SelectPage pageToActivate)
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
                        RecipeIngredientsPage ->
                            True

                        _ ->
                            False
                    )
                    RecipeIngredientsPage
                , tabListItem
                    "prepSteps-tab"
                    "prepSteps-content"
                    "Steps"
                    (case activePage of
                        RecipeStepsPage ->
                            True

                        _ ->
                            False
                    )
                    RecipeStepsPage
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


focus : String -> Cmd Msg
focus id =
    Browser.Dom.focus id
        |> Task.attempt (\_ -> NoOp)


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


ingredientsViewActions : Float -> Html Msg
ingredientsViewActions ratio =
    div
        [ class "w-100 d-flex justify-content-center"
        ]
        [ button
            [ type_ "button"
            , class "btn"
            , onClick ResetRecipeViewer
            , disabled (ratio == 1)
            ]
            [ ionIcon "refresh" 32
            ]
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


prepStepsViewActions : Int -> Int -> Html Msg
prepStepsViewActions indexToDisplay length =
    let
        prepStepButton : Msg -> Bool -> Html Msg -> Html Msg
        prepStepButton message isDisabled icon =
            div
                [ class "d-flex justify-content-center w-50" ]
                [ button
                    [ onClick message
                    , disabled isDisabled
                    , class "btn"
                    ]
                    [ icon ]
                ]
    in
    let
        btnSize =
            32
    in
    div
        [ class "d-flex w-100 m-auto gap-2"
        , style "max-width" "700px"
        ]
        [ prepStepButton
            Prev
            (indexToDisplay <= 0)
            (ionIcon "chevron-back-outline" btnSize)
        , prepStepButton
            Next
            (indexToDisplay >= length - 1)
            (ionIcon "chevron-forward-outline" btnSize)
        ]



{- All elements are rendered but displayed conditionally by visibility. By this the layout is fixed on the start and does not crash -}


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
