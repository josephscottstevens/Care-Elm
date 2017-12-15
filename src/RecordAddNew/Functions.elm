module RecordAddNew.Functions exposing (saveForm)

import Json.Encode as Encode
import Http
import RecordAddNew.Types exposing (Model)
import Common.Functions as Functions exposing (maybeVal, maybeToDateString)


encodeRecord : Model -> Int -> Encode.Value
encodeRecord newRecord patientId =
    Encode.object
        [ ( "RecordId", Encode.int <| newRecord.recordId )
        , ( "PatientId", Encode.int <| patientId )
        , ( "Title", Encode.string <| newRecord.title )
        , ( "RecordTypeId", Encode.int <| Functions.getId newRecord.recordType )
        , ( "Specialty", Encode.string <| newRecord.specialty )
        , ( "Provider", Encode.string <| newRecord.provider )
        , ( "TimeVisit", maybeVal Encode.string <| maybeToDateString <| newRecord.newRecord.timeVisit )
        , ( "TimeAcc", maybeVal Encode.string <| maybeToDateString <| newRecord.newRecord.timeAcc )
        , ( "RecordFile", Encode.string <| newRecord.newRecord.fileName )
        , ( "Comments", Encode.string <| newRecord.comments )
        , ( "FacilityId", maybeVal Encode.int <| newRecord.newRecord.facilityId )
        , ( "ReportDate", maybeVal Encode.string <| maybeToDateString <| newRecord.newRecord.reportDate )
        , ( "CallSid", Encode.string <| newRecord.callSid )
        , ( "RecordingSid", Encode.string <| newRecord.recording )
        , ( "RecordingDuration", Encode.int <| newRecord.duration )
        , ( "RecordingDate", maybeVal Encode.string <| maybeToDateString <| newRecord.newRecord.recordingDate )
        , ( "StaffId", maybeVal Encode.int <| newRecord.newRecord.userId )
        , ( "TaskId", maybeVal Encode.int <| newRecord.newRecord.taskId )

        -- Hospitilizations
        , ( "PatientReported", Encode.bool <| newRecord.patientReported )
        , ( "HospitalizationId", maybeVal Encode.int <| newRecord.newRecord.hospitalizationId )
        , ( "FacilityId2", maybeVal Encode.int <| newRecord.newRecord.facilityId2 )
        , ( "DateOfAdmission", maybeVal Encode.string <| maybeToDateString <| newRecord.newRecord.dateOfAdmission )
        , ( "DateOfDischarge", maybeVal Encode.string <| maybeToDateString <| newRecord.newRecord.dateOfDischarge )
        , ( "DateOfAdmission2", maybeVal Encode.string <| maybeToDateString <| newRecord.newRecord.dateOfAdmission2 )
        , ( "DateOfDischarge2", maybeVal Encode.string <| maybeToDateString <| newRecord.newRecord.dateOfDischarge2 )
        , ( "HospitalServiceTypeId", maybeVal Encode.int <| newRecord.newRecord.hospitalServiceTypeId )
        , ( "DischargeRecommendations", Encode.string <| newRecord.dischargeDiagnosis )
        , ( "DischargePhysicianId", maybeVal Encode.int <| newRecord.newRecord.dischargePhysicianId )
        , ( "AdmitDiagnosisId", maybeVal Encode.int <| newRecord.newRecord.admitDiagnosisId )
        , ( "DischargeDiagnosisId", maybeVal Encode.int <| newRecord.newRecord.dischargeDiagnosisId )
        ]


saveFormRequest : Model -> Int -> Http.Request String
saveFormRequest model patientId =
    Http.request
        { body = encodeRecord model patientId |> Http.jsonBody
        , expect = Http.expectString
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = "/People/AddNewRecord"
        , withCredentials = False
        }


saveForm : Model -> Int -> (Result Http.Error String -> msg) -> Cmd msg
saveForm model patientId saveCompleted =
    Http.send saveCompleted (saveFormRequest model patientId)
