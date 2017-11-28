module HospitilizationsAddEdit.Functions exposing (..)

import Json.Encode as Encode exposing (..)
import Http
import HospitilizationsAddEdit.Types exposing (..)
import Common.Types exposing (..)
import Common.Functions exposing (..)
import Ports


encodeRecord : Model -> Encode.Value
encodeRecord newRecord =
    Encode.object
        [ ( "Id", maybeVal Encode.int <| newRecord.id )
        , ( "PatientId", Encode.int <| newRecord.patientId )
        , ( "FacilityId", maybeVal Encode.int <| newRecord.facilityId )
        , ( "HospitalizationId", maybeVal Encode.int <| newRecord.hospitalizationId )
        , ( "FacilityId2", maybeVal Encode.int <| newRecord.facilityId2 )
        , ( "DateOfAdmission", maybeVal Encode.string <| maybeToDateString <| newRecord.dateOfAdmission )
        , ( "DateOfDischarge", maybeVal Encode.string <| maybeToDateString <| newRecord.dateOfDischarge )
        , ( "HospitalServiceTypeId", maybeVal Encode.int <| newRecord.hospitalServiceTypeId )
        , ( "DischargeRecommendations", Encode.string <| newRecord.dischargeRecommendations )
        , ( "DischargePhysicianId", maybeVal Encode.int <| newRecord.dischargePhysicianId )
        ]


saveFormRequest : Model -> Http.Request String
saveFormRequest model =
    Http.request
        { body = encodeRecord model |> Http.jsonBody
        , expect = Http.expectString
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = "/People/AddNewRecord"
        , withCredentials = False
        }


saveForm : Model -> Cmd Msg
saveForm model =
    Http.send SaveCompleted (saveFormRequest model)
