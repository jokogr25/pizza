module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Dom exposing (Error(..))
import Browser.Events
import Helper exposing (round2ToString, safeRegexOf)
import Html exposing (Html, button, div, i, img, input, label, span, text)
import Html.Attributes exposing (alt, attribute, class, classList, disabled, id, src, style, type_)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import List
import Regex
import String
import Svg.Attributes exposing (visibility)
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
    | RecipeViewer Recipe (Maybe Ingredient) Int (Maybe Float) Float ActivePage
    | RecipeCreator (List Recipe) Recipe (Maybe Ingredient)



-- MSG


type Msg
    = GoFront
    | GoCarousel
    | GoRecipeAlbum
    | GoRecipeViewer Recipe
    | GoRecipeCreator
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
    | UpdateLabel String
    | UpdateDescription String
    | UpdateImagePath String
    | AddIngredient
    | RemoveIngredient String
    | EditIngredient Ingredient
    | UpdateIngredientId String
    | UpdateIngredientLabel String
    | UpdateIngredientAmount String
    | UpdateIngredientUnit String
    | AddStep
    | UpdateStepTitle Int String
    | UpdateStepDescription Int String
    | SaveRecipe



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map
            (\key ->
                case key of
                    Enter ->
                        case model of
                            RecipeViewer _ (Just _) _ (Just newAmount) _ _ ->
                                if newAmount >= 1 then
                                    CalculateRatio

                                else
                                    NoOp

                            _ ->
                                NoOp

                    Left ->
                        case model of
                            RecipeViewer _ _ _ _ _ RecipeStepsPage ->
                                Prev

                            _ ->
                                NoOp

                    Right ->
                        case model of
                            RecipeViewer _ _ _ _ _ RecipeStepsPage ->
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
            ( Front, Cmd.none )

        GoCarousel ->
            ( Carousel, Cmd.none )

        GoRecipeViewer recipe ->
            ( RecipeViewer
                recipe
                Nothing
                0
                Nothing
                1
                RecipeIngredientsPage
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

        GoRecipeCreator ->
            case model of
                RecipeAlbum recipes _ ->
                    ( RecipeCreator
                        recipes
                        { id = "draft-id"
                        , label = "Draft label"
                        , description = "Draft description"
                        , ingredients = []
                        , steps = []
                        , image = Path ""
                        }
                        Nothing
                    , Cmd.none
                    )

                _ ->
                    noChange

        SelectPage page ->
            case model of
                RecipeViewer recipe maybeIngredient prepStepIndex maybeAmount ratio _ ->
                    ( RecipeViewer
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
                RecipeViewer recipe _ prepStepIndex _ ratio page ->
                    ( RecipeViewer
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
                RecipeViewer recipe _ prepStepIndex _ ratio page ->
                    ( RecipeViewer
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
                RecipeViewer recipe _ _ _ _ page ->
                    ( RecipeViewer
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
                RecipeViewer recipe maybeIngredient prepStepIndex _ ratio page ->
                    ( RecipeViewer
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
                RecipeViewer recipe maybeIngredient prepStepIndex maybeNewAmount _ page ->
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
                RecipeViewer recipe _ prepStepIndex _ ratio page ->
                    ( RecipeViewer
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
                RecipeViewer recipe maybeIngredient prepStepIndex maybeAmount ratio page ->
                    ( RecipeViewer
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
                RecipeViewer recipe maybeIngredient prepStepIndex maybeAmount ratio page ->
                    ( RecipeViewer
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

        UpdateLabel label ->
            case model of
                RecipeCreator recipes draft ingredientDraft ->
                    ( RecipeCreator
                        recipes
                        { draft
                            | label = label
                        }
                        ingredientDraft
                    , Cmd.none
                    )

                _ ->
                    noChange

        UpdateDescription description ->
            case model of
                RecipeCreator recipes draft ingredientDraft ->
                    ( RecipeCreator
                        recipes
                        { draft
                            | description = description
                        }
                        ingredientDraft
                    , Cmd.none
                    )

                _ ->
                    noChange

        UpdateImagePath imagePath ->
            case model of
                RecipeCreator recipes draft ingredientDraft ->
                    ( RecipeCreator
                        recipes
                        { draft
                            | image = Path imagePath
                        }
                        ingredientDraft
                    , Cmd.none
                    )

                _ ->
                    noChange

        AddIngredient ->
            case model of
                RecipeCreator recipes draft maybeIngredientDraft ->
                    ( RecipeCreator
                        recipes
                        { draft
                            | ingredients =
                                draft.ingredients
                                    ++ (case maybeIngredientDraft of
                                            Just ingredient ->
                                                if validateIngredient ingredient draft.ingredients then
                                                    [ ingredient ]

                                                else
                                                    []

                                            Nothing ->
                                                []
                                       )
                        }
                        (maybeIngredientDraft
                            |> Maybe.andThen
                                (\i ->
                                    if not (validateIngredient i draft.ingredients) then
                                        Just i

                                    else
                                        Nothing
                                )
                        )
                    , Cmd.none
                    )

                _ ->
                    noChange

        EditIngredient ing ->
            case model of
                RecipeCreator recipes draft _ ->
                    ( RecipeCreator
                        recipes
                        { draft
                            | ingredients =
                                List.filter (\i -> i.id /= ing.id) draft.ingredients
                        }
                        (Just ing)
                    , Cmd.none
                    )

                _ ->
                    noChange

        RemoveIngredient id ->
            case model of
                RecipeCreator recipes draft ingredientDraft ->
                    ( RecipeCreator
                        recipes
                        { draft
                            | ingredients =
                                List.filter
                                    (\ingredient ->
                                        ingredient.id /= id
                                    )
                                    draft.ingredients
                        }
                        ingredientDraft
                    , Cmd.none
                    )

                _ ->
                    noChange

        UpdateIngredientId newId ->
            case model of
                RecipeCreator recipes draft maybeIngredientDraft ->
                    ( RecipeCreator
                        recipes
                        draft
                        (case maybeIngredientDraft of
                            Just ing ->
                                Just
                                    { ing
                                        | id = newId
                                    }

                            Nothing ->
                                Just
                                    { id = newId
                                    , label = ""
                                    , amount = 0
                                    , unit = Gram
                                    }
                        )
                    , Cmd.none
                    )

                _ ->
                    noChange

        UpdateIngredientLabel newLabel ->
            case model of
                RecipeCreator recipes draft maybeIngredientDraft ->
                    ( RecipeCreator
                        recipes
                        draft
                        (case maybeIngredientDraft of
                            Just ing ->
                                Just
                                    { ing
                                        | label = newLabel
                                    }

                            Nothing ->
                                Just
                                    { id = ""
                                    , label = newLabel
                                    , amount = 0
                                    , unit = Gram
                                    }
                        )
                    , Cmd.none
                    )

                _ ->
                    noChange

        UpdateIngredientAmount newStrAmount ->
            case model of
                RecipeCreator recipes draft maybeIngredientDraft ->
                    case String.toFloat newStrAmount of
                        Just f ->
                            ( RecipeCreator
                                recipes
                                draft
                                (case maybeIngredientDraft of
                                    Just ing ->
                                        Just
                                            { ing
                                                | amount = f
                                            }

                                    Nothing ->
                                        Just
                                            { id = ""
                                            , label = ""
                                            , amount = f
                                            , unit = Gram
                                            }
                                )
                            , Cmd.none
                            )

                        Nothing ->
                            noChange

                _ ->
                    noChange

        UpdateIngredientUnit gUnit ->
            let
                parsedUnit : Unit
                parsedUnit =
                    Debug.log gUnit
                        (Maybe.withDefault Gram (parseUnit gUnit))

                default =
                    Just
                        { id = ""
                        , label = ""
                        , amount = 1.0
                        , unit = parsedUnit
                        }
            in
            case model of
                RecipeCreator recipes draft maybeIngredientDraft ->
                    ( RecipeCreator
                        recipes
                        draft
                        (case maybeIngredientDraft of
                            Just i ->
                                Just { i | unit = parsedUnit }

                            Nothing ->
                                default
                        )
                    , Cmd.none
                    )

                _ ->
                    noChange

        AddStep ->
            case model of
                RecipeCreator recipes draft ingredientDraft ->
                    ( RecipeCreator
                        recipes
                        { draft
                            | steps =
                                draft.steps
                                    ++ [ { time = 42
                                         , title = "New Step Title"
                                         , description = "New Step Description"
                                         }
                                       ]
                        }
                        ingredientDraft
                    , Cmd.none
                    )

                _ ->
                    noChange

        UpdateStepTitle index newTitle ->
            case model of
                RecipeCreator recipes draft ingredientDraft ->
                    ( RecipeCreator
                        recipes
                        { draft
                            | steps =
                                List.indexedMap
                                    (\i step ->
                                        if i == index then
                                            { step
                                                | title = newTitle
                                            }

                                        else
                                            step
                                    )
                                    draft.steps
                        }
                        ingredientDraft
                    , Cmd.none
                    )

                _ ->
                    noChange

        UpdateStepDescription index newDescription ->
            case model of
                RecipeCreator recipes draft ingredientDraft ->
                    ( RecipeCreator
                        recipes
                        { draft
                            | steps =
                                List.indexedMap
                                    (\i step ->
                                        if i == index then
                                            { step
                                                | description = newDescription
                                            }

                                        else
                                            step
                                    )
                                    draft.steps
                        }
                        ingredientDraft
                    , Cmd.none
                    )

                _ ->
                    noChange

        SaveRecipe ->
            case model of
                RecipeCreator recipes draft _ ->
                    ( RecipeAlbum
                        (draft :: recipes)
                        Nothing
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
            contentView
                RecipeAlbumPage
                (recipeAlbumView
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
                )
                Nothing

        RecipeViewer recipe selectedIngredient prepStepIndex maybeNewAmount ratio page ->
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

        RecipeCreator _ recipeDraft maybeIngredientDraft ->
            contentView
                RecipeCreatorPage
                (recipeCreatorView recipeDraft maybeIngredientDraft)
                (Just
                    (recipeCreatorActions
                        (validateRecipe recipeDraft)
                    )
                )


validateRecipe : Recipe -> Bool
validateRecipe recipe =
    validateIngredients recipe.ingredients
        && validateSteps recipe.steps


validateIngredient : Ingredient -> List Ingredient -> Bool
validateIngredient ing ings =
    let
        listOfIds =
            List.map (\i -> i.id) ings
    in
    not (String.isEmpty ing.id)
        && not (List.member ing.id listOfIds)
        && ing.amount
        > 0
        && not (String.isEmpty ing.label)


validateIngredients : List Ingredient -> Bool
validateIngredients ingredients =
    not
        (List.isEmpty ingredients)
        && uniqueStrings
            (List.map
                (\ingredient -> ingredient.id)
                ingredients
            )
        && List.all
            (\i -> i.amount > 0)
            ingredients


validateSteps : List PrepStep -> Bool
validateSteps steps =
    not
        (List.isEmpty steps)


uniqueStrings : List String -> Bool
uniqueStrings list =
    List.length list
        == List.length
            (List.foldl
                (\id unique ->
                    if List.member id unique then
                        unique

                    else
                        unique ++ [ id ]
                )
                []
                list
            )


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
        [ class "p-3 border-top d-flex justify-content-around"
        ]
        [ actions ]


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
        [ class "navbar navbar-expand-sm bg-body-tertiary"
        ]
        [ div
            [ class "container-fluid" ]
            [ Html.a
                [ class "navbar-brand"
                , onClick GoFront
                ]
                [ pizzaIcon ]
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


recipeAlbumView : List Recipe -> Html Msg
recipeAlbumView recipes =
    div []
        [ div
            [ class "album py-5" ]
            [ div
                [ class "container"
                , style "max-width" "700px"
                ]
                [ div
                    [ class "row row-cols-1 row-cols-sm-2 row-cols-md-3 g-3" ]
                    (List.map recipeAlbumCardView recipes
                        ++ [ div
                                [ class "col"
                                ]
                                [ div
                                    [ class "card shadow-sm h-100 d-flex justify-content-center align-items-center"
                                    , style "min-width" "220px"
                                    , style "min-height" "128px"
                                    , onClick GoRecipeCreator
                                    ]
                                    [ plusIcon 64
                                    ]
                                ]
                           ]
                    )
                ]
            ]
        ]


recipeAlbumCardView : Recipe -> Html Msg
recipeAlbumCardView recipe =
    div
        [ class "col" ]
        [ div
            [ class "card shadow-sm h-100"
            , style "min-width" "220px"
            ]
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
                                , onClick (GoRecipeViewer recipe)
                                ]
                                [ text "Open" ]
                        ]
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
        []
        [ div
            [ class "mx-auto my-3 my-md-4 px-3 px-md-0"
            , style "max-width" "700px"
            ]
            [ div
                []
                [ Html.h1
                    []
                    [ text recipe.label ]
                , Html.ul
                    [ class "nav nav-underline"
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
                    [ class "tab-content"
                    , id "recipeTabsContent"
                    , style "height" "60vh"
                    , style "overflow-y" "auto"
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
        [ class "btn-group"
        , attribute "role" "group"
        ]
        [ button
            [ type_ "button"
            , class "btn btn-primary"
            , onClick ResetRecipeViewer
            , disabled (ratio == 1)
            ]
            [ resetIcon
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
            if isSelected then
                if isNewAmountValid then
                    inputButton CalculateRatio checkIcon

                else
                    inputButton Abort closeIcon

            else
                inputButton (SelectIngredient ingredient) pencilIcon
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
            button
                [ onClick message
                , disabled isDisabled
                , class "btn btn-primary"
                ]
                [ icon ]
    in
    div
        [ class "btn-group gap-2"
        , Html.Attributes.attribute "role" "group"
        ]
        [ prepStepButton
            Prev
            (indexToDisplay <= 0)
            arrowLeftIcon
        , prepStepButton
            Next
            (indexToDisplay >= length - 1)
            arrowRightIcon
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


recipeCreatorView : Recipe -> Maybe Ingredient -> Html Msg
recipeCreatorView draft maybeIngredientToEdit =
    let
        addButton msg =
            button
                [ class "btn btn-outline-secondary btn-sm w-100 d-flex justify-content-center align-items-center mt-3"
                , onClick msg
                ]
                [ plusIcon 16 ]
    in
    div
        [ class "container my-4 flex-grow-1"
        , style "max-width" "700px"
        ]
        [ Html.h2
            []
            [ text "Create recipe" ]

        -- Recipe label
        , div
            [ class "mb-3"
            ]
            [ label
                [ class "form-label" ]
                [ text "Name" ]
            , input
                [ class "form-control"
                , Html.Attributes.value draft.label
                , onInput UpdateLabel
                ]
                []
            ]

        -- Description
        , div
            [ class "mb-3" ]
            [ label
                [ class "form-label" ]
                [ text "Description" ]
            , Html.textarea
                [ class "form-control"
                , Html.Attributes.rows 3
                , Html.Attributes.value draft.description
                , onInput UpdateDescription
                ]
                []
            ]

        -- Image
        , div
            [ class "mb-4" ]
            [ label [ class "form-label" ] [ text "Image path" ]
            , input
                [ class "form-control"
                , Html.Attributes.value (getPathStr draft.image)
                , onInput UpdateImagePath
                ]
                []
            ]

        -- Ingredients
        , Html.h4
            [ class "mt-4"
            ]
            [ text "Ingredients"
            ]
        , div
            []
            (List.map
                (\ing ->
                    div
                        [ class "d-flex align-items-center justify-content-between border rounded p-2 mb-2 position-relative"
                        ]
                        [ div
                            []
                            [ text ing.id ]
                        , button
                            [ class
                                "btn btn-sm btn-warn"
                            , Html.Attributes.title "Edit ingredient"
                            , onClick (EditIngredient ing)
                            ]
                            [ pencilIcon
                            ]
                        , button
                            [ class
                                "btn btn-sm btn-danger"
                            , onClick (RemoveIngredient ing.id)
                            , Html.Attributes.title "Remove ingredient"
                            ]
                            [ closeIcon
                            ]
                        ]
                )
                draft.ingredients
                ++ [ addOrEditIngredientView maybeIngredientToEdit
                   , addButton AddIngredient
                   ]
            )

        -- Steps
        , Html.h4
            [ class "mt-4"
            ]
            [ text "Steps"
            ]
        , div
            []
            (List.indexedMap addStepRow draft.steps
                ++ [ addButton AddStep
                   ]
            )
        ]


recipeCreatorActions : Bool -> Html Msg
recipeCreatorActions isRecipeValid =
    div
        [ class "btn-group"
        , attribute "role" "group"
        ]
        [ button
            [ type_ "button"
            , class "btn btn-primary"
            , disabled (not isRecipeValid)
            , onClick SaveRecipe
            ]
            [ saveIcon
            ]
        ]


addOrEditIngredientView : Maybe Ingredient -> Html Msg
addOrEditIngredientView maybeIngredient =
    let
        idValue =
            empyStyleMapper
                maybeIngredient
                (\ing -> Html.Attributes.value ing.id)

        labelValue =
            empyStyleMapper
                maybeIngredient
                (\ing -> Html.Attributes.value ing.label)

        amountValue =
            empyStyleMapper
                maybeIngredient
                (\ing -> Html.Attributes.value (String.fromFloat ing.amount))

        colInput l v message =
            div
                [ class "col-md-3" ]
                [ div
                    [ class "form-floating"
                    ]
                    [ input
                        [ class "form-control"
                        , onInput message
                        , v
                        ]
                        []
                    , label
                        []
                        [ text l ]
                    ]
                ]

        colSelect l message =
            div
                [ class "col-md-3"
                ]
                [ div
                    [ class "form-floating" ]
                    [ Html.select
                        [ class "form-select"
                        , onInput message
                        ]
                        (List.map
                            (\unit ->
                                Html.option
                                    [ Html.Attributes.value (unitToAbbr unit)
                                    , Html.Attributes.selected
                                        (case maybeIngredient of
                                            Just i ->
                                                i.unit == unit

                                            Nothing ->
                                                False
                                        )
                                    ]
                                    [ text (unitToAbbr unit) ]
                            )
                            allUnits
                        )
                    , label
                        []
                        [ text l
                        ]
                    ]
                ]
    in
    div
        [ class "mb-3 position-relative"
        ]
        [ div
            [ class "row g-2"
            ]
            [ colInput
                "Id"
                idValue
                UpdateIngredientId
            , colInput
                "Label"
                labelValue
                UpdateIngredientLabel
            , colInput
                "Amount"
                amountValue
                UpdateIngredientAmount
            , colSelect
                "Unit"
                UpdateIngredientUnit
            ]
        ]


addStepRow : Int -> PrepStep -> Html Msg
addStepRow index step =
    div
        [ class "card mb-2"
        ]
        [ div
            [ class "card-body"
            ]
            [ input
                [ class "form-control mb-2"
                , Html.Attributes.placeholder "Step title"
                , Html.Attributes.value step.title
                , onInput (UpdateStepTitle index)
                ]
                []
            , Html.textarea
                [ class "form-control"
                , Html.Attributes.rows 2
                , Html.Attributes.placeholder "Step description"
                , Html.Attributes.value step.description
                , onInput (UpdateStepDescription index)
                ]
                []
            ]
        ]



-- TYPES


type Unit
    = Gram
    | Mililiter
    | Teaspoon


allUnits : List Unit
allUnits =
    [ Gram
    , Mililiter
    , Teaspoon
    ]


unitToAbbr : Unit -> String
unitToAbbr unit =
    case unit of
        Gram ->
            "g"

        Mililiter ->
            "ml"

        Teaspoon ->
            "tsp"


parseUnit : String -> Maybe Unit
parseUnit s =
    case s of
        "g" ->
            Just Gram

        "ml" ->
            Just Mililiter

        "tsp" ->
            Just Teaspoon

        _ ->
            Nothing


type alias Recipe =
    { id : String
    , label : String
    , description : String
    , ingredients : List Ingredient
    , steps : List PrepStep
    , image : Path
    }


type alias PrepStep =
    { time : Int
    , title : String
    , description : String
    }


type Path
    = Path String


getPathStr : Path -> String
getPathStr p =
    case p of
        Path str ->
            str


type alias Ingredient =
    { id : String
    , label : String
    , amount : Float
    , unit : Unit
    }



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


empyStyleMapper : Maybe a -> (a -> Html.Attribute msg) -> Html.Attribute msg
empyStyleMapper m f =
    m
        |> Maybe.map f
        |> Maybe.withDefault emptyStyle


emptyStyle : Html.Attribute msg
emptyStyle =
    Html.Attributes.style "" ""


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
