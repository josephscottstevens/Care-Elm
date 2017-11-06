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



-- encodeRecord : Record -> Json.Encode.Value
-- encodeRecord record =
--     Json.Encode.object
--         [ ( "iD", Json.Encode.int <| record.iD )
--         , ( "date", Json.Encode.string <| record.date )
--         , ( "speciality", Json.Encode.maybe <| Json.Encode.string <| record.speciality )
--         , ( "comments", Json.Encode.string <| record.comments )
--         , ( "transferedTo", Json.Encode.string <| record.transferedTo )
--         , ( "transferedOn", Json.Encode.maybe <| encodeComplexType <| record.transferedOn )
--         , ( "patientID", Json.Encode.int <| record.patientID )
--         , ( "title", Json.Encode.maybe <| encodeComplexType <| record.title )
--         , ( "dateAccessioned", Json.Encode.maybe <| encodeComplexType <| record.dateAccessioned )
--         , ( "provider", Json.Encode.maybe <| encodeComplexType <| record.provider )
--         , ( "patientName", Json.Encode.maybe <| encodeComplexType <| record.patientName )
--         , ( "recordType", Json.Encode.string <| record.recordType )
--         , ( "dateOfAdmission", Json.Encode.maybe <| encodeComplexType <| record.dateOfAdmission )
--         , ( "dateOfDischarge", Json.Encode.maybe <| encodeComplexType <| record.dateOfDischarge )
--         , ( "dischargePhysician", Json.Encode.string <| record.dischargePhysician )
--         , ( "dischargeDiagnosis", Json.Encode.string <| record.dischargeDiagnosis )
--         , ( "hospitalizationServiceType", Json.Encode.string <| record.hospitalizationServiceType )
--         , ( "hospitalizationModel", Json.Encode.maybe <| encodeComplexType <| record.hospitalizationModel )
--         , ( "hospitalizationID", Json.Encode.maybe <| encodeComplexType <| record.hospitalizationID )
--         , ( "reportDate", Json.Encode.maybe <| encodeComplexType <| record.reportDate )
--         , ( "fileName", Json.Encode.string <| record.fileName )
--         , ( "canTransfer", Json.Encode.bool <| record.canTransfer )
--         , ( "facility", Json.Encode.maybe <| encodeComplexType <| record.facility )
--         , ( "facilityFax", Json.Encode.maybe <| encodeComplexType <| record.facilityFax )
--         , ( "recommendations", Json.Encode.maybe <| encodeComplexType <| record.recommendations )
--         ]


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


saveRequest : NewRecord -> Cmd Msg
saveRequest newRecord =
    Http.send DeleteCompleted <| Http.getString ("/People/AddNewRecord?recordModelString=" ++ (toString newRecord))


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
