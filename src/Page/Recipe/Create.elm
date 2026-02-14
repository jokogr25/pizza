module Page.Recipe.Create exposing (..)

import Domain.ActionButton exposing (ActionButton(..))
import Domain.Helper exposing (..)
import Domain.Icon exposing (ionIcon)
import Domain.Recipe as Recipe
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.Recipe.Album exposing (OutMsg)
import Platform.Cmd as Cmd


type alias Model =
    { recipes : List Recipe.Recipe
    , draft : Recipe.Recipe
    , removedIngredients : List Recipe.Ingredient
    , removedSteps : List Recipe.PrepStep
    , modal : Maybe Modal
    , edit : Edit
    }


type Edit
    = None
    | Ingredient Recipe.Ingredient
    | PrepStep Recipe.PrepStep


type Msg
    = UpdateRecipeLabel String
    | UpdateRecipeDescription String
    | UpdateRecipeImagePath String
      -- Ingredient
    | AddIngredient
    | RemoveIngredient Recipe.Ingredient
    | EditIngredient Recipe.Ingredient
    | UpdateIngredientId String
    | UpdateIngredientLabel String
    | UpdateIngredientAmount String
    | UpdateIngredientUnit String
      -- Step
    | AddStep
    | RemoveStep Recipe.PrepStep
    | EditStep Recipe.PrepStep
    | UpdateStepTitle String
    | UpdateStepTime String
    | UpdateStepDescription String
      --
    | OpenConfirmModal Msg
    | Confirm
    | Abort
    | NoOp
      --
    | Out OutMsg


type OutMsg
    = SaveRecipe Recipe.Recipe


type Modal
    = ConfirmModal Msg


init : List Recipe.Recipe -> Model
init recipes =
    { recipes = recipes
    , draft = recipeDraft
    , removedIngredients = []
    , removedSteps = []
    , modal = Nothing
    , edit = None
    }


initWithRecipe : Recipe.Recipe -> List Recipe.Recipe -> Model
initWithRecipe r l =
    { recipes = l
    , draft = r
    , removedIngredients = []
    , removedSteps = []
    , modal = Nothing
    , edit = None
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noChange =
            ( model, Cmd.none )
    in
    case msg of
        UpdateRecipeLabel label ->
            ( { model
                | draft = Recipe.updateLabel label model.draft
              }
            , Cmd.none
            )

        UpdateRecipeDescription description ->
            ( { model
                | draft = Recipe.updateDescription description model.draft
              }
            , Cmd.none
            )

        UpdateRecipeImagePath imagePath ->
            ( { model
                | draft = Recipe.updateImage (Recipe.Path imagePath) model.draft
              }
            , Cmd.none
            )

        AddIngredient ->
            case model.edit of
                Ingredient ing ->
                    if validateIngredient ing model.draft.ingredients then
                        ( { model
                            | draft =
                                Recipe.addIngredient
                                    ing
                                    model.draft
                            , edit = None
                          }
                        , Cmd.none
                        )

                    else
                        noChange

                _ ->
                    noChange

        EditIngredient ing ->
            ( { model
                | edit = Ingredient ing
                , draft = Recipe.removeIngredient ing model.draft
              }
            , Cmd.none
            )

        RemoveIngredient ing ->
            ( { model
                | draft = Recipe.removeIngredient ing model.draft
                , removedIngredients = ing :: model.removedIngredients
              }
            , Cmd.none
            )

        UpdateIngredientId newId ->
            case model.edit of
                Ingredient ing ->
                    ( { model
                        | edit = Ingredient (Recipe.updateIngredientId newId ing)
                      }
                    , Cmd.none
                    )

                _ ->
                    noChange

        UpdateIngredientLabel newLabel ->
            case model.edit of
                Ingredient ing ->
                    ( { model
                        | edit = Ingredient (Recipe.updateIngredientLabel newLabel ing)
                      }
                    , Cmd.none
                    )

                _ ->
                    noChange

        UpdateIngredientAmount newStrAmount ->
            case model.edit of
                Ingredient ing ->
                    case String.toFloat newStrAmount of
                        Just f ->
                            if f > 0 then
                                ( { model
                                    | edit = Ingredient (Recipe.updateIngredientAmount f ing)
                                  }
                                , Cmd.none
                                )

                            else
                                noChange

                        Nothing ->
                            noChange

                _ ->
                    noChange

        UpdateIngredientUnit gUnit ->
            case model.edit of
                Ingredient ing ->
                    let
                        parsedUnit : Recipe.Unit
                        parsedUnit =
                            Maybe.withDefault Recipe.Gram (Recipe.parseUnit gUnit)
                    in
                    ( { model
                        | edit = Ingredient (Recipe.updateIngredientUnit parsedUnit ing)
                      }
                    , Cmd.none
                    )

                _ ->
                    noChange

        AddStep ->
            case model.edit of
                PrepStep step ->
                    ( { model
                        | draft = Recipe.addPrepStep step model.draft
                        , edit = None
                      }
                    , Cmd.none
                    )

                _ ->
                    noChange

        UpdateStepTitle newTitle ->
            case model.edit of
                PrepStep step ->
                    ( { model
                        | edit = PrepStep (Recipe.updatePrepStepTitle newTitle step)
                      }
                    , Cmd.none
                    )

                _ ->
                    noChange

        UpdateStepDescription newDescription ->
            case model.edit of
                PrepStep step ->
                    ( { model
                        | edit = PrepStep (Recipe.updatePrepStepDescription newDescription step)
                      }
                    , Cmd.none
                    )

                _ ->
                    noChange

        UpdateStepTime newTimeStr ->
            case model.edit of
                PrepStep step ->
                    case String.toInt newTimeStr of
                        Just newTime ->
                            if newTime > 0 then
                                ( { model
                                    | edit = PrepStep (Recipe.updatePrepStepTime newTime step)
                                  }
                                , Cmd.none
                                )

                            else
                                noChange

                        Nothing ->
                            noChange

                _ ->
                    noChange

        EditStep step ->
            case model.edit of
                None ->
                    ( { model
                        | edit = PrepStep step
                        , removedSteps = step :: model.removedSteps
                      }
                    , Cmd.none
                    )

                _ ->
                    noChange

        RemoveStep step ->
            case model.edit of
                None ->
                    ( { model | draft = Recipe.removePrepStep step model.draft }, Cmd.none )

                _ ->
                    noChange

        -- m is the message that will be sent after clicking confirm button
        OpenConfirmModal m ->
            case model.modal of
                Nothing ->
                    ( { model
                        | modal = Just (ConfirmModal m)
                      }
                    , Cmd.none
                    )

                _ ->
                    noChange

        Confirm ->
            case model.modal of
                Just (ConfirmModal m) ->
                    update m model

                _ ->
                    noChange

        Abort ->
            case model.modal of
                Just (ConfirmModal _) ->
                    ( { model
                        | modal = Nothing
                      }
                    , Cmd.none
                    )

                _ ->
                    noChange

        Out outMsg ->
            case outMsg of
                SaveRecipe _ ->
                    noChange

        NoOp ->
            noChange


validateRecipe : Recipe.Recipe -> Bool
validateRecipe recipe =
    validateIngredients recipe.ingredients
        && validateSteps recipe.steps


validateStep : Recipe.PrepStep -> Bool
validateStep step =
    step.time
        >= -1
        && not (String.isEmpty step.title)
        && not (String.isEmpty step.description)


validateIngredient : Recipe.Ingredient -> List Recipe.Ingredient -> Bool
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


validateIngredients : List Recipe.Ingredient -> Bool
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


validateSteps : List Recipe.PrepStep -> Bool
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
    div
        []
        [ recipeCreatorView
            model.draft
            (case model.edit of
                Ingredient i ->
                    Just i

                _ ->
                    Nothing
            )
            (case model.edit of
                PrepStep i ->
                    Just i

                _ ->
                    Nothing
            )
        , model.modal
            |> Maybe.map (\_ -> confirmModalView)
            |> Maybe.withDefault (text "")
        ]


recipeCreatorView : Recipe.Recipe -> Maybe Recipe.Ingredient -> Maybe Recipe.PrepStep -> Html Msg
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
        [ let
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
                    , onInput UpdateRecipeLabel
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
                    , onInput UpdateRecipeDescription
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
                    , Html.Attributes.value (Recipe.getPathStr draft.image)
                    , onInput UpdateRecipeImagePath
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
        , ingredientsAddedView draft.ingredients
        , Html.hr
            [ class "border border-2 border-light"
            ]
            []
        , editIngredientView maybeIngredientToEdit
        , addButton AddIngredient (not isIngredientValid)

        -- Steps
        , Html.h4
            [ class "mt-4"
            ]
            [ text "Steps"
            ]
        , stepsAddedView draft.steps
        , Html.hr
            [ class "border border-2 border-light"
            ]
            []
        , editStepView maybePrepStepToEdit
        , addButton AddStep (not isStepValid)
        ]


ingredientsAddedView : List Recipe.Ingredient -> Html Msg
ingredientsAddedView ingredients =
    div []
        (List.map
            (\ing ->
                div
                    [ class "d-flex align-items-center justify-content-between border rounded p-2 mb-2" ]
                    [ div []
                        [ text ing.id ]
                    , div
                        [ class "d-flex gap-2" ]
                        [ button
                            [ class "btn btn-sm btn-outline action-btn-danger"
                            , Html.Attributes.title "Remove ingredient"
                            , onClick (OpenConfirmModal (RemoveIngredient ing))
                            ]
                            [ ionIcon "close" 20 ]
                        , button
                            [ class "btn btn-sm btn-outline action-btn-warning"
                            , Html.Attributes.title "Edit ingredient"
                            , onClick (EditIngredient ing)
                            ]
                            [ ionIcon "pencil" 20 ]
                        ]
                    ]
            )
            ingredients
        )


editIngredientView : Maybe Recipe.Ingredient -> Html Msg
editIngredientView maybeIngredient =
    let
        idValue =
            empyStyleMapper maybeIngredient (\ing -> Html.Attributes.value ing.id)

        labelValue =
            empyStyleMapper maybeIngredient (\ing -> Html.Attributes.value ing.label)

        amountValue =
            empyStyleMapper maybeIngredient
                (\ing ->
                    if ing.amount > 0 then
                        Html.Attributes.value (String.fromFloat ing.amount)

                    else
                        emptyStyle
                )

        colInput classes l v message =
            div
                [ class classes ]
                [ div [ class "form-floating" ]
                    [ input
                        [ class "form-control"
                        , onInput message
                        , v
                        ]
                        []
                    , label [] [ text l ]
                    ]
                ]

        colSelect classes l message =
            div
                [ class classes ]
                [ div [ class "form-floating" ]
                    [ Html.select
                        [ class "form-select"
                        , onInput message
                        ]
                        (List.map
                            (\unit ->
                                Html.option
                                    [ Html.Attributes.value (Recipe.unitToAbbr unit)
                                    , Html.Attributes.selected
                                        (case maybeIngredient of
                                            Just i ->
                                                i.unit == unit

                                            Nothing ->
                                                False
                                        )
                                    ]
                                    [ text (Recipe.unitToAbbr unit) ]
                            )
                            Recipe.allUnits
                        )
                    , label
                        []
                        [ text l ]
                    ]
                ]
    in
    div
        [ class "mb-3"
        ]
        [ div
            [ class "row g-2"
            ]
            [ colInput "col-12 col-md-9" "Label" labelValue UpdateIngredientLabel
            , colInput "col-12 col-md-3" "Id" idValue UpdateIngredientId
            ]
        , div
            [ class "row g-2 mt-1"
            ]
            [ colInput "col-9" "Amount" amountValue UpdateIngredientAmount
            , colSelect "col-3" "Unit" UpdateIngredientUnit
            ]
        ]


stepsAddedView : List Recipe.PrepStep -> Html Msg
stepsAddedView steps =
    div []
        (List.map
            (\step ->
                div
                    [ class "d-flex align-items-center justify-content-between border rounded p-2 mb-2" ]
                    [ div []
                        [ text step.title ]
                    , div
                        [ class "d-flex gap-2" ]
                        [ button
                            [ class "btn btn-sm btn-outline action-btn-danger"
                            , Html.Attributes.title "Remove step"
                            , onClick (RemoveStep step)
                            ]
                            [ ionIcon "close" 20 ]
                        , button
                            [ class "btn btn-sm btn-outline action-btn-warning"
                            , Html.Attributes.title "Edit step"
                            , onClick (EditStep step)
                            ]
                            [ ionIcon "pencil" 20 ]
                        ]
                    ]
            )
            steps
        )


editStepView : Maybe Recipe.PrepStep -> Html Msg
editStepView maybeStep =
    let
        stepTitleValue =
            empyStyleMapper maybeStep (\step -> Html.Attributes.value step.title)

        stepTimeValue =
            empyStyleMapper maybeStep
                (\step ->
                    if step.time > -1 then
                        Html.Attributes.value (String.fromInt step.time)

                    else
                        emptyStyle
                )

        stepDescriptionValue =
            empyStyleMapper maybeStep (\step -> Html.Attributes.value step.description)

        colInput classes l v message =
            div
                [ class classes ]
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
                        [ text l
                        ]
                    ]
                ]

        colTextArea l v message =
            div
                [ class "col-12"
                ]
                [ div
                    [ class "form-floating"
                    ]
                    [ Html.textarea
                        [ class "form-control"
                        , style "min-height" "120px"
                        , Html.Attributes.rows 6
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
        [ class "mb-3"
        ]
        [ div
            [ class "row g-2"
            ]
            [ colInput "col-12 col-md-9"
                "Title"
                stepTitleValue
                UpdateStepTitle
            , colInput
                "col-12 col-md-3"
                "Time"
                stepTimeValue
                UpdateStepTime
            ]
        , div
            [ class "row g-2 mt-1"
            ]
            [ colTextArea "Description"
                stepDescriptionValue
                UpdateStepDescription
            ]
        ]


confirmModalView : Html Msg
confirmModalView =
    div []
        [ div
            [ class
                "modal-backdrop fade show"
            ]
            []
        , div
            [ class "modal fade show"
            , style "display" "block"
            ]
            [ div
                [ class "modal-dialog"
                ]
                [ div
                    [ class "modal-content"
                    ]
                    [ div
                        [ class "modal-header"
                        ]
                        [ h5
                            [ class "modal-title"
                            ]
                            [ text "Modal Title"
                            ]
                        , button
                            [ class "btn-close", onClick Abort ]
                            []
                        ]
                    , div
                        [ class "modal-body"
                        ]
                        [ text "This is the popup content!"
                        ]
                    , div
                        [ class "modal-footer"
                        ]
                        [ button
                            [ class "btn action-btn-light"
                            , onClick Abort
                            ]
                            [ text "Close" ]
                        , button
                            [ class "btn action-btn-danger"
                            , onClick Confirm
                            ]
                            [ text "Save Changes" ]
                        ]
                    ]
                ]
            ]
        ]


recipeDraft : Recipe.Recipe
recipeDraft =
    { id = ""
    , label = ""
    , description = ""
    , ingredients = []
    , steps = []
    , image = Recipe.Path ""
    }
