module Domain.Recipe exposing (..)

import Domain.Helper exposing (round2ToString, safeRegexOf, uniqueStrings)
import Regex


type alias Recipe =
    { id : String
    , label : String
    , description : String
    , image : Path
    , ingredients : List Ingredient
    , steps : List PrepStep
    }


updateId : String -> Recipe -> Recipe
updateId id recipe =
    { recipe
        | id = id
    }


updateLabel : String -> Recipe -> Recipe
updateLabel label recipe =
    { recipe
        | label = label
    }


updateDescription : String -> Recipe -> Recipe
updateDescription description recipe =
    { recipe
        | description = description
    }


updateImage : Path -> Recipe -> Recipe
updateImage path recipe =
    { recipe
        | image = path
    }


addIngredient : Ingredient -> Recipe -> Recipe
addIngredient ing recipe =
    { recipe
        | ingredients = recipe.ingredients ++ [ ing ]
    }


removeIngredient : Ingredient -> Recipe -> Recipe
removeIngredient ing recipe =
    { recipe
        | ingredients =
            List.filter
                (\i -> i.id /= ing.id)
                recipe.ingredients
    }


addPrepStep : PrepStep -> Recipe -> Recipe
addPrepStep step recipe =
    { recipe
        | steps = recipe.steps ++ [ step ]
    }


removePrepStep : PrepStep -> Recipe -> Recipe
removePrepStep step recipe =
    { recipe
        | steps =
            List.filter
                (\i -> i /= step)
                recipe.steps
    }


type alias PrepStep =
    { time : Int
    , title : String
    , description : String
    }


updatePrepStepTime : Int -> PrepStep -> PrepStep
updatePrepStepTime time step =
    { step
        | time = time
    }


updatePrepStepTitle : String -> PrepStep -> PrepStep
updatePrepStepTitle title step =
    { step
        | title = title
    }


updatePrepStepDescription : String -> PrepStep -> PrepStep
updatePrepStepDescription description step =
    { step
        | description = description
    }


type alias Ingredient =
    { id : String
    , label : String
    , amount : Float
    , unit : Unit
    }


updateIngredientId : String -> Ingredient -> Ingredient
updateIngredientId id ing =
    { ing
        | id = id
    }


updateIngredientLabel : String -> Ingredient -> Ingredient
updateIngredientLabel label ing =
    { ing
        | label = label
    }


updateIngredientAmount : Float -> Ingredient -> Ingredient
updateIngredientAmount amount ing =
    { ing
        | amount = amount
    }


updateIngredientUnit : Unit -> Ingredient -> Ingredient
updateIngredientUnit unit ing =
    { ing
        | unit = unit
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



-- sample data


samples =
    [ samplePizzaRecipe
    , sampleLasagneRecipe
    ]


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
