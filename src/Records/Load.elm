module Records.Load exposing (..)

import Json.Encode
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Http
import Records.Model exposing (..)
import Table


decodeRecord : Json.Decode.Decoder Record
decodeRecord =
    Json.Decode.Pipeline.decode Record
        |> Json.Decode.Pipeline.required "Id" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "Date" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "Speciality" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "Comments" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "TransferedTo" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "TransferedOn" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "PatientId" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "Title" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "DateAccessed" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "Provider" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "PatientName" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "RecordType" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "DateOfAdmission" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "DateOfDischarge" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "DischargePhysician" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "DischargeDiagnosis" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "HospitalizationServiceType" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "HospitalizationId" (Json.Decode.maybe Json.Decode.int)
        |> Json.Decode.Pipeline.required "ReportDate" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "FileName" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "CanTransfer" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "Facility" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "FacilityFax" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "Recommendations" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.hardcoded DropdownClosed
        |> hardcoded ( 0, 0 )
        |> hardcoded True



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


request : Http.Request Model
request =
    Http.get "/people/PatientRecordsGrid?patientId=6676" decodeModel


getRecords : (Result Http.Error Model -> msg) -> Cmd msg
getRecords t =
    Http.send t request


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
