module UpdateHelper exposing (..)

import Model exposing (..)


x =
    1



-- maybeEmployee : List Employer -> Int -> Maybe Employer
-- maybeEmployee employerArray i =
--     Array.get i employerArray
-- setEmployer : List Employer -> Int -> Employer -> List Employer
-- setEmployer employerArray i emp =
--     Array.set i emp employerArray
-- maybeUpdateState : Model -> Int -> String -> (Employer -> String -> Employer) -> List Employer
-- maybeUpdateState model i newState func =
--     case maybeEmployee model.employers i of
--         Just emp ->
--             setEmployer model.employers i (func emp newState)
--         Nothing ->
--             model.employers
-- updateEmployeeList : Model -> Int -> String -> (Employer -> String -> Employer) -> Model
-- updateEmployeeList model i newValue t =
--     { model | employers = (maybeUpdateState model i newValue t) }
