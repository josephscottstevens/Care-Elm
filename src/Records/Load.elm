module Records.Load exposing (..)

import Json.Encode
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Http
import Records.Model exposing (..)
import Table


decodeRecord : Decoder Record
decodeRecord =
    decode Record
        |> required "Id" (int)
        |> required "Date" (maybe string)
        |> required "Speciality" (maybe string)
        |> required "Comments" (maybe string)
        |> required "TransferedTo" (maybe string)
        |> required "TransferedOn" (maybe string)
        |> required "PatientId" (int)
        |> required "Title" (maybe string)
        |> required "DateAccessed" (maybe string)
        |> required "Provider" (maybe string)
        |> required "PatientName" (maybe string)
        |> required "RecordType" (maybe string)
        |> required "DateOfAdmission" (maybe string)
        |> required "DateOfDischarge" (maybe string)
        |> required "DischargePhysician" (maybe string)
        |> required "DischargeDiagnosis" (maybe string)
        |> required "HospitalizationServiceType" (maybe string)
        |> required "HospitalizationId" (maybe int)
        |> required "ReportDate" (maybe string)
        |> required "FileName" (maybe string)
        |> required "CanTransfer" (bool)
        |> required "Facility" (maybe string)
        |> required "FacilityFax" (maybe string)
        |> required "Recommendations" (maybe string)
        |> hardcoded False


type alias NewRecord =
    { facility : String
    , category : String
    , dateTimeOfVisit : String
    , doctorOfVisit : String
    , specialityOfVisit : String
    , comments : String
    , recordFile : String
    }


encodeRecord : NewRecord -> Json.Encode.Value
encodeRecord newRecord =
    Json.Encode.object
        [ ( "Facility", Json.Encode.string <| newRecord.facility )
        , ( "Category", Json.Encode.string <| newRecord.category )
        , ( "DateTimeOfVisit", Json.Encode.string <| newRecord.dateTimeOfVisit )
        , ( "DoctorOfVisit", Json.Encode.string <| newRecord.doctorOfVisit )
        , ( "SpecialityOfVisit", Json.Encode.string <| newRecord.specialityOfVisit )
        , ( "Comments", Json.Encode.string <| newRecord.comments )
        , ( "RecordFile", Json.Encode.string <| newRecord.recordFile )
        , ( "FacilityID", Json.Encode.int 79 )
        , ( "PatientID", Json.Encode.int 6676 )
        , ( "FileName", Json.Encode.string "test.xlsx" )
        ]


decodeModel : Decoder Model
decodeModel =
    decode Model
        |> hardcoded Initial
        |> required "list" (list decodeRecord)
        |> hardcoded (Table.initialSort "dob")
        |> hardcoded ""
        |> hardcoded emptyNewRecord


request : Http.Request Model
request =
    Http.get "/people/PatientRecordsGrid?patientId=6676" decodeModel


getRecords : (Result Http.Error Model -> msg) -> Cmd msg
getRecords t =
    Http.send t request


deleteRequest : Record -> Cmd Msg
deleteRequest record =
    Http.send DeleteCompleted <| Http.getString ("/records/DeleteRecord?recordId=" ++ (toString record.id))


updateRecords : List Record -> Record -> List Record
updateRecords records newRecord =
    records
        |> List.map
            (\t ->
                if t.id == newRecord.id then
                    newRecord
                else
                    t
            )


setFacility : String -> NewRecord -> NewRecord
setFacility newFacility newRecord =
    { newRecord | facility = newFacility }


setCategory : String -> NewRecord -> NewRecord
setCategory newCategory newRecord =
    { newRecord | category = newCategory }


setDateTimeOfVisit : String -> NewRecord -> NewRecord
setDateTimeOfVisit newDateTimeOfVisit newRecord =
    { newRecord | dateTimeOfVisit = newDateTimeOfVisit }


setDoctorOfVisit : String -> NewRecord -> NewRecord
setDoctorOfVisit newDoctorOfVisit newRecord =
    { newRecord | doctorOfVisit = newDoctorOfVisit }


setSpecialtyOfVisit : String -> NewRecord -> NewRecord
setSpecialtyOfVisit newSpecialityOfVisit newRecord =
    { newRecord | specialityOfVisit = newSpecialityOfVisit }


setComments : String -> NewRecord -> NewRecord
setComments newComments newRecord =
    { newRecord | comments = newComments }


setRecordFile : String -> NewRecord -> NewRecord
setRecordFile newRecordFile newRecord =
    { newRecord | recordFile = newRecordFile }
