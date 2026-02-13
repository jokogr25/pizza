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
import Page.Recipe.Album as RecipeAlbum exposing (Model(..), Msg(..))
import Page.Recipe.Create as RecipeCreate exposing (Model(..))
import Page.Recipe.View as RecipeView exposing (Model(..), Tab(..))
import Platform.Cmd as Cmd
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( Front samples, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Model
    = Front (List Recipe)
    | Carousel
      -- Pages
    | RecipeAlbum RecipeAlbum.Model
    | RecipeCreate RecipeCreate.Model
    | RecipeView RecipeView.Model



-- MSG


type Msg
    = GoFront
    | GoCarousel
    | GoRecipeAlbum
    | InputSearch String
    | NoOp
      -- SubComponents
    | AlbumMsg RecipeAlbum.Msg
    | CreateMsg RecipeCreate.Msg
    | ViewMsg RecipeView.Msg



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map
            (\key ->
                case key of
                    Enter ->
                        case model of
                            RecipeView (RecipeView.View _ _ (Just _) _ (Just newAmount) _ _) ->
                                if newAmount >= 1 then
                                    ViewMsg RecipeView.CalculateRatio

                                else
                                    NoOp

                            _ ->
                                NoOp

                    Left ->
                        case model of
                            RecipeView (RecipeView.View _ _ _ _ _ _ Steps) ->
                                ViewMsg RecipeView.Prev

                            _ ->
                                NoOp

                    Right ->
                        case model of
                            RecipeView (RecipeView.View _ _ _ _ _ _ Steps) ->
                                ViewMsg RecipeView.Next

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
                    ( Front recipes
                    , Cmd.none
                    )

                RecipeView (RecipeView.View recipes _ _ _ _ _ _) ->
                    ( Front recipes
                    , Cmd.none
                    )

                RecipeCreate (RecipeCreate.Create recipes _ _ _ _) ->
                    ( Front recipes
                    , Cmd.none
                    )

                _ ->
                    noChange

        GoCarousel ->
            ( Carousel, Cmd.none )

        GoRecipeAlbum ->
            case model of
                Front recipes ->
                    ( RecipeAlbum
                        (RecipeAlbum.Album
                            recipes
                            Nothing
                        )
                    , Cmd.none
                    )

                RecipeView (RecipeView.View recipes _ _ _ _ _ _) ->
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

                RecipeCreate (RecipeCreate.Create recipes _ _ _ _) ->
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
                                    ( RecipeView (RecipeView.init recipes recipe)
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
                RecipeCreate (RecipeCreate.Create recipes draft maybeIngredient maybeStep modal) ->
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
                                        (RecipeCreate.Create recipes draft maybeIngredient maybeStep modal)
                                    )
                                )
                            , Cmd.none
                            )

                _ ->
                    noChange

        ViewMsg viewMsg ->
            case model of
                RecipeView (RecipeView.View recipes recipe maybeIngredient stepIndex maybeAmount ratio tab) ->
                    let
                        m =
                            RecipeView.View recipes recipe maybeIngredient stepIndex maybeAmount ratio tab
                    in
                    case viewMsg of
                        RecipeView.SelectIngredient ing ->
                            ( RecipeView
                                (RecipeView.update (RecipeView.SelectIngredient ing) m)
                            , focus ing.id
                            )

                        RecipeView.OutMsg outMsg ->
                            case outMsg of
                                RecipeView.EditRecipe ->
                                    ( RecipeCreate
                                        (RecipeCreate.Create [] recipe Nothing Nothing Nothing)
                                    , Cmd.none
                                    )

                        _ ->
                            ( RecipeView
                                (RecipeView.update viewMsg m)
                            , Cmd.none
                            )

                _ ->
                    noChange

        InputSearch term ->
            case model of
                RecipeAlbum m ->
                    ( RecipeAlbum
                        (RecipeAlbum.update (InputAlbumSearch term) m)
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

        RecipeAlbum m ->
            contentView
                RecipeAlbumPage
                (RecipeAlbum.view m
                    |> Html.map AlbumMsg
                )

        RecipeCreate m ->
            contentView
                RecipeCreatorPage
                (RecipeCreate.view m
                    |> Html.map CreateMsg
                )

        RecipeView (View recipes recipe maybeIngredient stepIndex maybeAmount ratio tab) ->
            let
                currentTab =
                    case tab of
                        RecipeView.Ingredients ->
                            RecipeCreatorPage

                        RecipeView.Steps ->
                            RecipeCreatorPage
            in
            contentView
                currentTab
                (RecipeView.view (View recipes recipe maybeIngredient stepIndex maybeAmount ratio tab)
                    |> Html.map ViewMsg
                )


type ActivePage
    = RecipeAlbumPage
    | RecipeCreatorPage


contentView : ActivePage -> Html Msg -> Html Msg
contentView activePage content =
    div
        [ class "d-flex flex-column vh-100"
        ]
        [ navbarView activePage
        , div
            [ class "flex-grow-1 overflow-auto"
            ]
            [ content
            ]
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
                        , onInput InputSearch
                        ]
                        []
                    ]
                ]
            ]
        ]


focus : String -> Cmd Msg
focus id =
    Browser.Dom.focus id
        |> Task.attempt (\_ -> NoOp)


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
