module Load exposing (..)

import Model exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Http


decodeEmployer : Decoder Employer
decodeEmployer =
    decode Employer
        |> hardcoded 0
        |> required "Dob" string
        |> required "Email" string
        |> required "AddressLine1" string
        |> required "AddressLine2" string
        |> required "City" string
        |> required "State" string
        |> required "ZipCode" string
        |> required "Phone" string


decodeEmployent : Decoder Model
decodeEmployent =
    decode Model
        |> hardcoded Initial
        |> required "PatientId" int
        |> required "Employers" (list decodeEmployer)
        |> hardcoded (Table.initialSort "dob")
        |> hardcoded ""


request : Http.Request Model
request =
    Http.get "/People/GetEmploymentInfo?patientId=6676" decodeEmployent


getEmployment : Cmd Msg
getEmployment =
    Http.send Load request



-- Not good, rowId has to be patched on later, but I don't how to make it apart of the decoder


newEmployers : List Employer -> List Employer
newEmployers employers =
    employers |> List.indexedMap (\idx t -> { t | rowId = idx })


updateEmployers : List Employer -> Employer -> List Employer
updateEmployers employers newEmployer =
    employers
        |> List.map
            (\oldEmployer ->
                if oldEmployer.rowId == newEmployer.rowId then
                    newEmployer
                else
                    oldEmployer
            )
