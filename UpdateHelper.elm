module UpdateHelper exposing (..)

import Model exposing (..)
import Array


testDt : Array.Array Employer -> Int -> String
testDt employers idx =
    case Array.get idx employers of
        Just emp ->
            emp.startDate

        Nothing ->
            ""


newEmployerState : Employer -> String -> Employer
newEmployerState emp newState =
    { emp | state = newState }


maybeEmployee : Array.Array Employer -> Int -> Maybe Employer
maybeEmployee employerArray i =
    Array.get i employerArray


setEmployer : Array.Array Employer -> Int -> Employer -> Array.Array Employer
setEmployer employerArray i emp =
    Array.set i emp employerArray


maybeUpdateState : Model -> Int -> String -> (Employer -> String -> Employer) -> Array.Array Employer
maybeUpdateState model i newState func =
    case maybeEmployee model.employers i of
        Just emp ->
            setEmployer model.employers i (func emp newState)

        Nothing ->
            model.employers


updateEmployeeList : Model -> Int -> String -> (Employer -> String -> Employer) -> Model
updateEmployeeList model i newValue t =
    { model | employers = (maybeUpdateState model i newValue t) }
