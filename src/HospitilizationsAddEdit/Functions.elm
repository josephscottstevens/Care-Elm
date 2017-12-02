module HospitilizationsAddEdit.Functions exposing (..)

import Json.Encode as Encode exposing (..)
import Http
import HospitilizationsAddEdit.Types exposing (..)
import Common.Functions exposing (..)
import Common.Types exposing (HospitilizationsRow)


encodeRecord : Model -> Encode.Value
encodeRecord newRecord =
    Encode.object
        [ ( "Id", maybeVal Encode.int <| newRecord.id )
        , ( "PatientId", Encode.int <| newRecord.patientId )
        , ( "FacilityId", maybeVal Encode.int <| newRecord.facilityId )
        , ( "PatientReported", Encode.bool <| newRecord.patientReported )
        , ( "HospitalizationId", maybeVal Encode.int <| newRecord.hospitalizationId )
        , ( "DateOfAdmission", maybeVal Encode.string <| maybeToDateString <| newRecord.dateOfAdmission )
        , ( "DateOfDischarge", maybeVal Encode.string <| maybeToDateString <| newRecord.dateOfDischarge )
        , ( "HospitalServiceTypeId", maybeVal Encode.int <| newRecord.hospitalServiceTypeId )
        , ( "ChiefComplaint", Encode.string <| newRecord.chiefComplaint )
        , ( "AdmitDiagnosisId", maybeVal Encode.int <| newRecord.admitDiagnosisId )
        , ( "DischargeDiagnosisId", maybeVal Encode.int <| newRecord.dischargeDiagnosisId )
        , ( "DischargeRecommendations", Encode.string <| newRecord.dischargeRecommendations )
        , ( "DischargePhysicianId", maybeVal Encode.int <| newRecord.dischargePhysicianId )
        , ( "FacilityId2", maybeVal Encode.int <| newRecord.facilityId2 )
        , ( "DateOfAdmission2", maybeVal Encode.string <| maybeToDateString <| newRecord.dateOfAdmission2 )
        , ( "DateOfDischarge2", maybeVal Encode.string <| maybeToDateString <| newRecord.dateOfDischarge2 )
        ]


saveFormRequest : Model -> Http.Request String
saveFormRequest model =
    Http.request
        { body = encodeRecord model |> Http.jsonBody
        , expect = Http.expectString
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = "/People/AddNewHospitilization"
        , withCredentials = False
        }


saveForm : Model -> Cmd Msg
saveForm model =
    Http.send SaveCompleted (saveFormRequest model)


updateModel : Model -> HospitilizationsRow -> Model
updateModel model t =
    { model
        | patientId = t.patientId
        , facilityId = t.facilityId
        , patientReported = t.patientReported
        , hospitalizationId = t.hospitalizationId
        , dateOfAdmission = t.dateOfAdmission
        , dateOfDischarge = t.dateOfDischarge
        , hospitalServiceTypeId = t.hospitalServiceTypeId
        , chiefComplaint = t.chiefComplaint
        , admitDiagnosisId = t.admitDiagnosisId
        , dischargeDiagnosisId = t.dischargeDiagnosisId
        , dischargeRecommendations = t.dischargeRecommendations
        , dischargePhysicianId = t.dischargePhysicianId
        , facilityId2 = t.facilityId2
        , dateOfAdmission2 = t.dateOfAdmission2
        , dateOfDischarge2 = t.dateOfDischarge2
    }
