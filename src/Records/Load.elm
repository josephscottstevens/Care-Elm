module Billing.Load exposing (..)

import Json.Encode
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Http
import Billing.Model exposing (..)
import Table


type alias Record =
    { iD : Int
    , date : String
    , speciality : Maybe String
    , comments : String
    , transferedTo : String
    , transferedOn : Maybe String
    , patientID : Int
    , title : Maybe String
    , dateAccessioned : Maybe String
    , provider : Maybe String
    , patientName : Maybe String
    , recordType : String
    , dateOfAdmission : Maybe String
    , dateOfDischarge : Maybe String
    , dischargePhysician : String
    , dischargeDiagnosis : String
    , hospitalizationServiceType : String
    , hospitalizationModel : Maybe String
    , hospitalizationID : Maybe Int
    , reportDate : Maybe String
    , fileName : String
    , canTransfer : Bool
    , facility : Maybe String
    , facilityFax : Maybe String
    , recommendations : Maybe String
    }


decodeRecord : Json.Decode.Decoder Record
decodeRecord =
    Json.Decode.Pipeline.decode Record
        |> Json.Decode.Pipeline.required "ID" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "Date" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "Speciality" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "Comments" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "TransferedTo" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "TransferedOn" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "PatientID" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "Title" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "DateAccessioned" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "Provider" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "PatientName" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "RecordType" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "DateOfAdmission" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "DateOfDischarge" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "DischargePhysician" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "DischargeDiagnosis" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "HospitalizationServiceType" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "HospitalizationID" (Json.Decode.maybe Json.Decode.int)
        |> Json.Decode.Pipeline.required "ReportDate" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "FileName" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "CanTransfer" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "Facility" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "FacilityFax" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "Recommendations" (Json.Decode.maybe Json.Decode.string)



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
-- decodeModel : Decoder Model
-- decodeModel =
--     decode Model
--         |> hardcoded Initial
--         |> required "list" (list decodeBillingCcm)
--         |> hardcoded (Table.initialSort "dob")
--         |> hardcoded ""
--         |> hardcoded 0
-- request : Http.Request Model
-- request =
--     Http.get "/people/CcmGridDataSource?showOpenCcmBills=true" decodeModel
-- getEmployment : (Result Http.Error Model -> msg) -> Cmd msg
-- getEmployment t =
--     Http.send t request
-- -- Not good, rowId has to be patched on later, but I don't how to make it apart of the decoder
-- newEmployers : List BillingCcm -> List BillingCcm
-- newEmployers enrollment =
--     enrollment |> List.indexedMap (\idx t -> { t | iD = idx })
-- updateEmployers : List BillingCcm -> BillingCcm -> List BillingCcm
-- updateEmployers enrollment newEnrollment =
--     enrollment
--         |> List.map
--             (\t ->
--                 if t.iD == newEnrollment.iD then
--                     newEnrollment
--                 else
--                     t
--             )
