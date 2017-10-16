module Hello exposing (..)

import Json.Decode exposing (..)
import Http

type alias Employment =
    {
        patientId : Int
    ,   employer : String
    ,   occupation : String
    ,   startDate : String
    }
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

getEmployment : Http.Request Employment
getEmployment = Http.get "/People/GetEmploymentInfo?patientId=6676" nestedDecoder


-- tasks : List Task
-- tasks =
--     case decodeString decoder testData of
--         Err t -> []
--         Ok t -> t