module Load exposing (..)

import Model exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Http


decodeEmployer : Decoder Employer
decodeEmployer =
    decode Employer
        |> required "EmploymentStatus" string
        |> required "Occupation" string
        |> required "Employer" string
        |> required "StartDate" string
        |> required "EndDate" string
        |> required "ContactPerson" string
        |> required "Status" string
        |> required "AddressLine1" string
        |> required "AddressLine2" string
        |> required "City" string
        |> required "State" string
        |> required "ZipCode" string
        |> required "Phone" string
        |> required "Email" string
        |> required "Comment" string


decodeEmployent : Decoder Employment
decodeEmployent =
    decode Employment
        |> required "PatientId" int
        |> required "Employers" (list decodeEmployer)
        |> required "TestDate" string


request : Http.Request Employment
request =
    Http.get "/People/GetEmploymentInfo?patientId=6676" decodeEmployent


getEmployment : Cmd Msg
getEmployment =
    Http.send Load request
