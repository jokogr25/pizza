module Pizza.Model.Types exposing (..)


type Unit
    = Gram
    | Mililiter
    | Teaspoon


unitToAbbr : Unit -> String
unitToAbbr unit =
    case unit of
        Gram ->
            "g"

        Mililiter ->
            "ml"

        Teaspoon ->
            "tsp"


type alias Recipe =
    { id : String
    , label : String
    , description : String
    , ingredients : List Ingredient
    , steps : List PrepStep
    , image : RecipeImage
    }


type alias PrepStep =
    { time : Int
    , title : String
    , description : String
    }


type RecipeImage
    = Path String


type alias Ingredient =
    { id : String
    , label : String
    , amount : Float
    , unit : Unit
    }


recipeApplyRatio : Float -> Recipe -> Recipe
recipeApplyRatio ratio recipe =
    { recipe
        | ingredients =
            List.map (ingredientApplyRatio ratio) recipe.ingredients
    }


ingredientApplyRatio : Float -> Ingredient -> Ingredient
ingredientApplyRatio ratio ingredient =
    { ingredient
        | amount = ingredient.amount * ratio
    }
