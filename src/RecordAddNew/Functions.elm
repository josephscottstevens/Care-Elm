module RecordAddNew.Functions exposing (..)

import Json.Encode as Encode exposing (..)
import Http
import RecordAddNew.Types exposing (..)
import Common.Types exposing (..)
import Common.Functions exposing (..)


encodeRecord : Model -> Encode.Value
encodeRecord newRecord =
    Encode.object
        [ ( "RecordId", Encode.int <| newRecord.recordId )
        , ( "PatientId", Encode.int <| newRecord.patientId )
        , ( "Title", Encode.string <| newRecord.title )
        , ( "RecordTypeId", maybeVal Encode.int <| newRecord.recordTypeId )
        , ( "Specialty", Encode.string <| newRecord.specialty )
        , ( "Provider", Encode.string <| newRecord.provider )
        , ( "TimeVisit", maybeVal Encode.string <| maybeToDateString <| newRecord.timeVisit )
        , ( "TimeAcc", maybeVal Encode.string <| maybeToDateString <| newRecord.timeAcc )
        , ( "RecordFile", Encode.string <| newRecord.fileName )
        , ( "Comments", Encode.string <| newRecord.comments )
        , ( "FacilityId", maybeVal Encode.int <| newRecord.facilityId )
        , ( "ReportDate", maybeVal Encode.string <| maybeToDateString <| newRecord.reportDate )
        , ( "CallSid", Encode.string <| newRecord.callSid )
        , ( "RecordingSid", Encode.string <| newRecord.recording )
        , ( "RecordingDuration", Encode.int <| newRecord.duration )
        , ( "RecordingDate", maybeVal Encode.string <| maybeToDateString <| newRecord.recordingDate )
        , ( "StaffId", maybeVal Encode.int <| newRecord.userId )
        , ( "TaskId", maybeVal Encode.int <| newRecord.taskId )

        -- Hospitilizations
        , ( "PatientReported", Encode.bool <| newRecord.patientReported )
        , ( "HospitalizationId", maybeVal Encode.int <| newRecord.hospitalizationId )
        , ( "FacilityId2", maybeVal Encode.int <| newRecord.facilityId2 )
        , ( "DateOfAdmission", maybeVal Encode.string <| maybeToDateString <| newRecord.dateOfAdmission )
        , ( "DateOfDischarge", maybeVal Encode.string <| maybeToDateString <| newRecord.dateOfDischarge )
        , ( "DateOfAdmission2", maybeVal Encode.string <| maybeToDateString <| newRecord.dateOfAdmission2 )
        , ( "DateOfDischarge2", maybeVal Encode.string <| maybeToDateString <| newRecord.dateOfDischarge2 )
        , ( "HospitalServiceTypeId", maybeVal Encode.int <| newRecord.hospitalServiceTypeId )
        , ( "DischargeRecommendations", Encode.string <| newRecord.dischargeRecommendations )
        , ( "DischargePhysicianId", maybeVal Encode.int <| newRecord.dischargePhysicianId )
        , ( "AdmitDiagnosisId", maybeVal Encode.int <| newRecord.admitDiagnosisId )
        , ( "DischargeDiagnosisId", maybeVal Encode.int <| newRecord.dischargeDiagnosisId )
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


getAddEditMsg : Maybe AddEditDataSource -> Maybe Int -> Bool -> Bool -> InitRecordAddNew
getAddEditMsg addEditDataSource recordTypeId setFocus isExistingHospitilization =
    case addEditDataSource of
        Just t ->
            { facilityId = t.facilityId
            , facilities = t.facilities
            , recordTypes = t.recordTypes
            , users = t.users
            , tasks = t.tasks
            , hospitilizationServiceTypes = t.hospitilizationServiceTypes
            , hospitalizationDischargePhysicians = t.hospitalizationDischargePhysicians
            , hospitilizations = t.hospitilizations
            , recordTypeId = recordTypeId
            , setFocus = setFocus
            , isExistingHospitilization = isExistingHospitilization
            }

        Nothing ->
            Debug.crash "Datasource is required for records addedit"
