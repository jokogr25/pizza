module Page.Recipe.Create exposing (..)

import Domain.Recipe exposing (..)


type Model
    = Create (List Recipe) Recipe (Maybe Ingredient) (Maybe PrepStep)
