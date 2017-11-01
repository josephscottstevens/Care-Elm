module Load exposing (..)

import Json.Encode
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Http
import Model exposing (..)
import Table
import GridPaging


decodeBillingCcm : Json.Decode.Decoder BillingCcm
decodeBillingCcm =
    Json.Decode.Pipeline.decode BillingCcm
        |> Json.Decode.Pipeline.required "ID" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "Facility" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "FacilityId" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "PracticeLocation" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "MainProvider" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "ProviderId" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "PatientName" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "PatientId" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "DoB" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "PatientFacilityIdNo" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "Phone" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "AssignedTo" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "StaffId" (Json.Decode.maybe Json.Decode.int)
        |> Json.Decode.Pipeline.required "OpenTasks" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "TotalTimeSpent" (Json.Decode.maybe Json.Decode.int)
        |> Json.Decode.Pipeline.required "CcmRegistrationDate" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "DateOfService" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "BillingDate" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "BillingMonth" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "BillingYear" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "IsClosed" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "TocId" (Json.Decode.maybe Json.Decode.int)
        |> Json.Decode.Pipeline.required "Readmission" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "IsComplexCCM" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "BatchCloseOnInvoiceCompletion" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "ReviewedByStaffName" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "CanModifyReviewedStatus" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "CPT" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "IsReviewed" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "DxPresent" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "CarePlanPresent" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "MedsPresent" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "AllergiesPresent" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "VitalsPresent" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "RecordingPresent" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "ChartComplete" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "Status" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "Is24HoursSinceBilledString" (Json.Decode.string)



-- encodeBillingCcm : BillingCcm -> Json.Encode.Value
-- encodeBillingCcm record =
--     Json.Encode.object
--         [ ( "iD", Json.Encode.int <| record.iD )
--         , ( "facility", Json.Encode.string <| record.facility )
--         , ( "facilityId", Json.Encode.int <| record.facilityId )
--         , ( "practiceLocation", Json.Encode.string <| record.practiceLocation )
--         , ( "mainProvider", Json.Encode.string <| record.mainProvider )
--         , ( "providerId", Json.Encode.int <| record.providerId )
--         , ( "patientName", Json.Encode.string <| record.patientName )
--         , ( "patientId", Json.Encode.int <| record.patientId )
--         , ( "doB", Json.Encode.string <| record.doB )
--         , ( "patientFacilityIdNo", Json.Encode.string <| record.patientFacilityIdNo )
--         , ( "phone", Json.Encode.string <| record.phone )
--         , ( "assignedTo", Json.Encode.string <| record.assignedTo )
--         , ( "staffId", Json.Encode.int <| record.staffId )
--         , ( "openTasks", Json.Encode.int <| record.openTasks )
--         , ( "totalTimeSpent", Json.Encode.int <| record.totalTimeSpent )
--         , ( "ccmRegistrationDate", Json.Encode.string <| record.ccmRegistrationDate )
--         , ( "dateOfService", Json.Encode.string <| record.dateOfService )
--         , ( "billingDate", Json.Encode.string <| record.billingDate )
--         , ( "billingMonth", Json.Encode.int <| record.billingMonth )
--         , ( "billingYear", Json.Encode.int <| record.billingYear )
--         , ( "isClosed", Json.Encode.bool <| record.isClosed )
--         , ( "tocId", Json.Encode.int <| record.tocId )
--         , ( "readmission", Json.Encode.bool <| record.readmission )
--         , ( "isComplexCCM", Json.Encode.bool <| record.isComplexCCM )
--         , ( "batchCloseOnInvoiceCompletion", Json.Encode.bool <| record.batchCloseOnInvoiceCompletion )
--         , ( "reviewedByStaffName", Json.Encode.string <| record.reviewedByStaffName )
--         , ( "canModifyReviewedStatus", Json.Encode.bool <| record.canModifyReviewedStatus )
--         , ( "cPT", Json.Encode.string <| record.cPT )
--         , ( "isReviewed", Json.Encode.bool <| record.isReviewed )
--         , ( "dxPresent", Json.Encode.bool <| record.dxPresent )
--         , ( "carePlanPresent", Json.Encode.bool <| record.carePlanPresent )
--         , ( "medsPresent", Json.Encode.bool <| record.medsPresent )
--         , ( "allergiesPresent", Json.Encode.bool <| record.allergiesPresent )
--         , ( "vitalsPresent", Json.Encode.bool <| record.vitalsPresent )
--         , ( "recordingPresent", Json.Encode.bool <| record.recordingPresent )
--         , ( "chartComplete", Json.Encode.bool <| record.chartComplete )
--         , ( "status", Json.Encode.string <| record.status )
--         , ( "is24HoursSinceBilledString", Json.Encode.string <| record.is24HoursSinceBilledString )
--         ]


decodeModel : Decoder Model
decodeModel =
    decode Model
        |> hardcoded Initial
        |> required "list" (list decodeBillingCcm)
        |> hardcoded (Table.initialSort "dob")
        |> hardcoded ""
        |> hardcoded GridPaging.initialPageState


request : Http.Request Model
request =
    Http.get "/people/CcmGridDataSource?showOpenCcmBills=true" decodeModel


getEmployment : Cmd Msg
getEmployment =
    Http.send Load request



-- Not good, rowId has to be patched on later, but I don't how to make it apart of the decoder


newEmployers : List BillingCcm -> List BillingCcm
newEmployers enrollment =
    enrollment |> List.indexedMap (\idx t -> { t | iD = idx })


updateEmployers : List BillingCcm -> BillingCcm -> List BillingCcm
updateEmployers enrollment newEnrollment =
    enrollment
        |> List.map
            (\t ->
                if t.iD == newEnrollment.iD then
                    newEnrollment
                else
                    t
            )
