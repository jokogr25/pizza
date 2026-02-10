module Api.SimpleMockApi exposing (..)

import Domain.Recipe exposing (Recipe, samples, validateRecipe)


type ApiError
    = Message String
    | UserDoesNotExist
    | SessionInvalid


type alias User =
    { name : String
    , session : String
    }


login : String -> Result ApiError User
login name =
    userExist name
        |> Maybe.map Ok
        |> Maybe.withDefault (Err UserDoesNotExist)


getRecipes : String -> Result ApiError (List Recipe)
getRecipes session =
    if isSessionValid session then
        Ok samples

    else
        Err SessionInvalid


saveRecipe : String -> Recipe -> Result ApiError Recipe
saveRecipe session recipe =
    if isSessionValid session then
        if validateRecipe recipe then
            Ok recipe

        else
            Err (Message "Error")

    else
        Err SessionInvalid



--


userExist : String -> Maybe User
userExist name =
    List.filter
        (\user -> user.name == name)
        existingUsers
        |> List.head


isSessionValid : String -> Bool
isSessionValid session =
    List.any
        (\user -> user.session == session)
        existingUsers


existingUsers : List User
existingUsers =
    [ User "Joscha" "1337" ]
