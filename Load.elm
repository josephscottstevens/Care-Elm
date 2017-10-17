module Load exposing (..)

import Model exposing (..)
import Json.Decode exposing (..)
import Http


nestedDecoder : Decoder Employment
nestedDecoder =
    map4 Employment
        (field "PatientId" int)
        (field "Employer" string)
        (field "Occupation" string)
        (field "StartDate" string)



-- decoder : Decoder (List Task)
-- decoder =
--   at ["data"] (list nestedDecoder)


request : Http.Request Employment
request =
    Http.get "https://localhost:44336/People/GetEmploymentInfo?patientId=6676" nestedDecoder


getEmployment : Cmd Msg
getEmployment =
    Http.send Load request



-- tasks : List Task
-- tasks =
--     case decodeString decoder testData of
--         Err t -> []
--         Ok t -> t
