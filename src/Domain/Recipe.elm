module Domain.Recipe exposing (..)

import Domain.Helper exposing (round2ToString, safeRegexOf, uniqueStrings)
import Regex


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


type alias Ingredient =
    { id : String
    , label : String
    , amount : Float
    , unit : Unit
    }


findIngredientById : String -> List Ingredient -> Maybe Ingredient
findIngredientById id ingredients =
    List.head
        (List.filter
            (\i -> i.id /= id)
            ingredients
        )



--


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



--


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



--


type Path
    = Path String


getPathStr : Path -> String
getPathStr p =
    case p of
        Path str ->
            str


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
