module HospitilizationsAddEdit.Functions exposing (..)

import Json.Encode as Encode exposing (..)
import Http
import HospitilizationsAddEdit.Types exposing (..)
import Common.Functions exposing (..)
import Common.Types exposing (AddEditDataSource, RecordAddNewInitData)


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


getHospitilizationMsg : AddEditDataSource -> Maybe Int -> RecordAddNewInitData -> InitHospitilizationsAddNew
getHospitilizationMsg addEditDataSource hospitilizationId t =
    { facilityId = addEditDataSource.facilityId
    , facilities = addEditDataSource.facilities
    , hospitilizationServiceTypes = addEditDataSource.hospitilizationServiceTypes
    , hospitalizationDischargePhysicians = addEditDataSource.hospitalizationDischargePhysicians
    , hospitilizations = addEditDataSource.hospitilizations
    , hospitilizationId = hospitilizationId

    -- for edit
    , patientId = t.patientId
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



-- { facilityId : Maybe Int
-- , facilities : List DropDownItem
-- , hospitilizationServiceTypes : List DropDownItem
-- , hospitalizationDischargePhysicians : List DropDownItem
-- , hospitilizations : List DropDownItem
-- , hospitilizationId : Maybe Int
-- -- for edit
-- , patientId : Int
-- , facilityText : String
-- , patientReported : Bool
-- , hospitalizationId : Maybe Int
-- , hospitalizationText : String
-- , hospitalServiceTypeId : Maybe Int
-- , hospitalServiceTypeText : String
-- , chiefComplaint : String
-- , admitDiagnosisId : Maybe Int
-- , dischargeDiagnosisId : Maybe Int
-- , dischargeRecommendations : String
-- , dischargePhysicianId : Maybe Int
-- , dischargePhysicianText : String
-- , facilityId2 : Maybe Int
-- , facilityText2 : String
-- , dateOfAdmission2 : Maybe String
-- , dateOfDischarge2 : Maybe String
-- }
