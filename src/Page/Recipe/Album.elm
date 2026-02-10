module Page.Recipe.Album exposing (..)

import Domain.Helper exposing (emptyStyle)
import Domain.Icon exposing (addIcon)
import Domain.Recipe exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type Model
    = Album (List Recipe) (Maybe String)


type Msg
    = InputAlbumSearch String
    | Out OutMsg
    | NoOp


type OutMsg
    = GoRecipeViewer Recipe
    | GoRecipeCreator


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputAlbumSearch str ->
            case model of
                Album recipes _ ->
                    Album recipes (Just str)

        _ ->
            model


view : Model -> Html Msg
view model =
    case model of
        Album recipes maybeSearch ->
            let
                maybeFilteredRecipes =
                    maybeSearch
                        |> Maybe.map
                            (\m ->
                                List.filter
                                    (\recipe ->
                                        List.any
                                            (String.contains m)
                                            [ recipe.label, recipe.id ]
                                    )
                                    recipes
                            )
                        |> Maybe.withDefault recipes
            in
            recipeAlbumView maybeFilteredRecipes


recipeAlbumView : List Recipe -> Html Msg
recipeAlbumView recipes =
    let
        addButtonCard =
            div [ class "col" ]
                [ div
                    [ class "card shadow-sm h-100 rounded-4"
                    , style "min-width" "220px"
                    , onClick (Out GoRecipeCreator)
                    ]
                    [ div
                        [ class "card-img-top"
                        , style "height" "225px"
                        ]
                        []
                    , div
                        [ class "card-body d-flex justify-content-center align-items-center" ]
                        [ addIcon 64 ]
                    ]
                ]
    in
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
                        ++ [ addButtonCard
                           ]
                    )
                ]
            ]
        ]


recipeAlbumCardView : Recipe -> Html Msg
recipeAlbumCardView recipe =
    let
        isRecipeValid =
            not (List.isEmpty recipe.ingredients && List.isEmpty recipe.steps)
    in
    div
        [ class "col" ]
        [ div
            [ class "card shadow-sm h-100"
            , style "min-width" "220px"
            , if isRecipeValid then
                onClick (Out (GoRecipeViewer recipe))

              else
                emptyStyle
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
                    []
                ]
            ]
        ]
