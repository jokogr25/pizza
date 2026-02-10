module Page.Recipe.Create exposing (..)

import Domain.Helper exposing (..)
import Domain.Icon exposing (ionIcon)
import Domain.Recipe as Recipe exposing (Ingredient, Path(..), PrepStep, Recipe, Unit(..), allUnits, getPathStr, parseUnit, unitToAbbr)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.Recipe.Album exposing (OutMsg)
import Platform.Cmd as Cmd


type Model
    = Create (List Recipe) Recipe (Maybe Ingredient) (Maybe PrepStep)


type Msg
    = UpdateLabel String
    | UpdateDescription String
    | UpdateImagePath String
      -- Ingredient
    | AddIngredient
    | RemoveIngredient String
    | EditIngredient Ingredient
    | UpdateIngredientId String
    | UpdateIngredientLabel String
    | UpdateIngredientAmount String
    | UpdateIngredientUnit String
      -- Step
    | AddStep
    | RemoveStep PrepStep
    | EditStep PrepStep
    | UpdateStepTitle String
    | UpdateStepTime String
    | UpdateStepDescription String
      --
    | NoOp
      --
    | Out OutMsg


type OutMsg
    = SaveRecipe Recipe


init : List Recipe -> Model
init recipes =
    Create
        recipes
        { id = ""
        , label = ""
        , description = ""
        , ingredients = []
        , steps = []
        , image = Recipe.Path ""
        }
        Nothing
        Nothing


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    let
        noChange =
            ( model, Cmd.none )
    in
    case msg of
        UpdateLabel label ->
            case model of
                Create recipes draft ingredientDraft stepDraft ->
                    ( Create
                        recipes
                        { draft
                            | label = label
                        }
                        ingredientDraft
                        stepDraft
                    , Cmd.none
                    )

        UpdateDescription description ->
            case model of
                Create recipes draft ingredientDraft stepDraft ->
                    ( Create
                        recipes
                        { draft
                            | description = description
                        }
                        ingredientDraft
                        stepDraft
                    , Cmd.none
                    )

        UpdateImagePath imagePath ->
            case model of
                Create recipes draft ingredientDraft stepDraft ->
                    ( Create
                        recipes
                        { draft
                            | image = Path imagePath
                        }
                        ingredientDraft
                        stepDraft
                    , Cmd.none
                    )

        AddIngredient ->
            case model of
                Create recipes draft maybeIngredientDraft maybeStepDraft ->
                    ( Create
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
                        maybeStepDraft
                    , Cmd.none
                    )

        EditIngredient ing ->
            case model of
                Create recipes draft _ maybeStepDraft ->
                    ( Create
                        recipes
                        { draft
                            | ingredients =
                                List.filter (\i -> i.id /= ing.id) draft.ingredients
                        }
                        (Just ing)
                        maybeStepDraft
                    , Cmd.none
                    )

        RemoveIngredient id ->
            case model of
                Create recipes draft ingredientDraft maybeStepDraft ->
                    ( Create
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
                        maybeStepDraft
                    , Cmd.none
                    )

        UpdateIngredientId newId ->
            case model of
                Create recipes draft maybeIngredientDraft maybeStepDraft ->
                    ( Create
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
                        maybeStepDraft
                    , Cmd.none
                    )

        UpdateIngredientLabel newLabel ->
            case model of
                Create recipes draft maybeIngredientDraft maybeStepDraft ->
                    ( Create
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
                        maybeStepDraft
                    , Cmd.none
                    )

        UpdateIngredientAmount newStrAmount ->
            case model of
                Create recipes draft maybeIngredientDraft maybeStepDraft ->
                    case String.toFloat newStrAmount of
                        Just f ->
                            ( Create
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
                                maybeStepDraft
                            , Cmd.none
                            )

                        Nothing ->
                            noChange

        UpdateIngredientUnit gUnit ->
            let
                parsedUnit : Unit
                parsedUnit =
                    Debug.log gUnit
                        (Maybe.withDefault Gram (parseUnit gUnit))
            in
            case model of
                Create recipes draft maybeIngredientDraft maybeStepDraft ->
                    ( Create
                        recipes
                        draft
                        (case maybeIngredientDraft of
                            Just i ->
                                Just { i | unit = parsedUnit }

                            Nothing ->
                                Just
                                    { id = ""
                                    , label = ""
                                    , amount = 0
                                    , unit = parsedUnit
                                    }
                        )
                        maybeStepDraft
                    , Cmd.none
                    )

        AddStep ->
            case model of
                Create recipes draft maybeIngredientDraft maybeStepDraft ->
                    ( Create
                        recipes
                        { draft
                            | steps =
                                draft.steps
                                    ++ (case maybeStepDraft of
                                            Just step ->
                                                [ step ]

                                            Nothing ->
                                                []
                                       )
                        }
                        maybeIngredientDraft
                        maybeStepDraft
                    , Cmd.none
                    )

        UpdateStepTitle newTitle ->
            case model of
                Create recipes draft maybeIngredientDraft maybeStepDraft ->
                    ( Create
                        recipes
                        draft
                        maybeIngredientDraft
                        (case maybeStepDraft of
                            Just step ->
                                Just
                                    { step
                                        | title = newTitle
                                    }

                            Nothing ->
                                Just
                                    { title = newTitle
                                    , time = -1
                                    , description = ""
                                    }
                        )
                    , Cmd.none
                    )

        UpdateStepDescription newDescription ->
            case model of
                Create recipes draft maybeIngredientDraft maybeStepDraft ->
                    ( Create
                        recipes
                        draft
                        maybeIngredientDraft
                        (case maybeStepDraft of
                            Just step ->
                                Just
                                    { step
                                        | description = newDescription
                                    }

                            Nothing ->
                                Just
                                    { description = newDescription
                                    , time = -1
                                    , title = ""
                                    }
                        )
                    , Cmd.none
                    )

        UpdateStepTime newTime ->
            let
                parsedTime : Int
                parsedTime =
                    String.toInt newTime |> Maybe.withDefault -1
            in
            case model of
                Create recipes draft maybeIngredientDraft maybeStepDraft ->
                    ( Create
                        recipes
                        draft
                        maybeIngredientDraft
                        (case maybeStepDraft of
                            Just step ->
                                Just
                                    { step
                                        | time = parsedTime
                                    }

                            Nothing ->
                                Just
                                    { time = parsedTime
                                    , description = "newDescription"
                                    , title = ""
                                    }
                        )
                    , Cmd.none
                    )

        EditStep step ->
            case model of
                Create recipes draft maybeIngredient _ ->
                    ( Create
                        recipes
                        { draft
                            | steps =
                                List.filter
                                    (\s -> s /= step)
                                    draft.steps
                        }
                        maybeIngredient
                        (Just step)
                    , Cmd.none
                    )

        RemoveStep step ->
            case model of
                Create recipes draft maybeIngredient _ ->
                    ( Create
                        recipes
                        { draft
                            | steps =
                                List.filter
                                    (\s -> s /= step)
                                    draft.steps
                        }
                        maybeIngredient
                        Nothing
                    , Cmd.none
                    )

        Out outMsg ->
            case outMsg of
                SaveRecipe _ ->
                    noChange

        NoOp ->
            noChange


validateRecipe : Recipe -> Bool
validateRecipe recipe =
    validateIngredients recipe.ingredients
        && validateSteps recipe.steps


validateStep : PrepStep -> Bool
validateStep step =
    step.time
        >= -1
        && not (String.isEmpty step.title)
        && not (String.isEmpty step.description)


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


view : Model -> Html Msg
view model =
    case model of
        Create _ recipeDraft maybeIngredientDraft maybeStepDraft ->
            recipeCreatorView
                recipeDraft
                maybeIngredientDraft
                maybeStepDraft


recipeCreatorView : Recipe -> Maybe Ingredient -> Maybe PrepStep -> Html Msg
recipeCreatorView draft maybeIngredientToEdit maybePrepStepToEdit =
    let
        isIngredientValid =
            maybeIngredientToEdit
                |> Maybe.map (\i -> validateIngredient i draft.ingredients)
                |> Maybe.withDefault False

        isStepValid =
            maybePrepStepToEdit
                |> Maybe.map (\i -> validateStep i)
                |> Maybe.withDefault False

        addButton msg isDisabled =
            button
                [ class "btn btn-sm w-100 d-flex justify-content-center align-items-center mt-3"
                , disabled isDisabled
                , onClick msg
                ]
                [ ionIcon "add" 32 ]
    in
    div
        [ class "container my-4 flex-grow-1"
        , style "max-width" "700px"
        ]
        [ Html.h2
            []
            [ text "Create recipe" ]
        , let
            id =
                "createRecipeLabel"
          in
          div
            [ class "col-md-12 mb-3"
            ]
            [ div
                [ class "form-floating"
                ]
                [ input
                    [ class "form-control"
                    , Html.Attributes.value id
                    , Html.Attributes.value draft.label
                    , onInput UpdateLabel
                    ]
                    []
                , label
                    [ Html.Attributes.for id
                    ]
                    [ text "Name"
                    ]
                ]
            ]
        , let
            id =
                "createRecipeDescription"
          in
          div
            [ class "col-md-12 mb-3" ]
            [ div
                [ class "form-floating"
                ]
                [ Html.textarea
                    [ class "form-control"
                    , Html.Attributes.id id
                    , Html.Attributes.rows 3
                    , Html.Attributes.value draft.description
                    , onInput UpdateDescription
                    ]
                    []
                , label
                    [ Html.Attributes.for id
                    ]
                    [ text "Description"
                    ]
                ]
            ]
        , let
            id =
                "createRecipeImagePath"
          in
          div
            [ class "col-md-12 mb-4" ]
            [ div
                [ class "form-floating"
                ]
                [ input
                    [ class "form-control"
                    , Html.Attributes.id id
                    , Html.Attributes.value (getPathStr draft.image)
                    , onInput UpdateImagePath
                    ]
                    []
                , label
                    [ Html.Attributes.for id ]
                    [ text "Image path" ]
                ]
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
                            [ ionIcon "pencil" 32
                            ]
                        , button
                            [ class
                                "btn btn-sm btn-danger"
                            , onClick (RemoveIngredient ing.id)
                            , Html.Attributes.title "Remove ingredient"
                            ]
                            [ ionIcon "close" 32
                            ]
                        ]
                )
                draft.ingredients
                ++ [ addOrEditIngredientView maybeIngredientToEdit
                   , addButton AddIngredient (not isIngredientValid)
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
            (List.map
                (\step ->
                    div
                        [ class "d-flex align-items-center justify-content-between border rounded p-2 mb-2 position-relative"
                        ]
                        [ div
                            []
                            [ text step.title
                            ]
                        , button
                            [ class "btn btn-sm btn-warn"
                            , Html.Attributes.title "Edit step"
                            , onClick (EditStep step)
                            ]
                            [ ionIcon "pencil" 32
                            ]
                        , button
                            [ class "btn btn-sm btn-danger"
                            , onClick (RemoveStep step)
                            , Html.Attributes.title "Remove step"
                            ]
                            [ ionIcon "close" 32
                            ]
                        ]
                )
                draft.steps
                ++ [ addOrEditStepView maybePrepStepToEdit
                   , addButton AddStep (not isStepValid)
                   ]
            )
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
                (\ing ->
                    if ing.amount > 0 then
                        Html.Attributes.value (String.fromFloat ing.amount)

                    else
                        emptyStyle
                )

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


addOrEditStepView : Maybe PrepStep -> Html Msg
addOrEditStepView maybeStep =
    let
        stepTitleValue =
            empyStyleMapper
                maybeStep
                (\step -> Html.Attributes.value step.title)

        stepTimeValue =
            empyStyleMapper
                maybeStep
                (\step ->
                    if step.time > -1 then
                        Html.Attributes.value (String.fromInt step.time)

                    else
                        emptyStyle
                )

        stepDescriptionValue =
            empyStyleMapper
                maybeStep
                (\step ->
                    Html.Attributes.value step.description
                )

        colInput l v message =
            div
                [ class "col-md-12"
                ]
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

        colTextArea l v message =
            div
                [ class "col-md-12"
                ]
                [ div
                    [ class "form-floating"
                    ]
                    [ Html.textarea
                        [ class "form-control"
                        , Html.Attributes.rows 2
                        , v
                        , onInput message
                        ]
                        []
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
                "Title"
                stepTitleValue
                UpdateStepTitle
            , colTextArea
                "Description"
                stepDescriptionValue
                UpdateStepDescription
            , colInput
                "Time"
                stepTimeValue
                UpdateStepTime
            ]
        ]
