port module Billing exposing (Msg, Model, emptyModel, subscriptions, init, update, view)

import Html exposing (Html, div, input, program, button, select, option, span, a)
import Html.Attributes exposing (style, class, placeholder, id, type_, value, tabindex)
import Html.Events exposing (onClick, onInput)
import Common.ServerTable as Table exposing (stringColumn, dateColumn)
import Common.Functions as Functions exposing (maybeVal, defaultString)
import Common.Types exposing (AddEditDataSource)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


init : Int -> Cmd Msg
init patientId =
    load patientId <| Table.init emptyRow "facility"


subscriptions : Sub msg
subscriptions =
    Sub.none


type alias Model =
    { rows : List Row
    , gridOperations : Table.GridOperations Row
    , query : String
    }


type alias Row =
    { id : Int
    , facility : Maybe String
    , facilityId : Int
    , practiceLocation : Maybe String
    , mainProvider : Maybe String
    , providerId : Int
    , patientName : Maybe String
    , patientId : Int
    , dob : Maybe String
    , patientFacilityIdNo : Maybe String
    , phone : Maybe String
    , assignedTo : Maybe String
    , staffId : Maybe Int
    , openTasks : Int
    , totalTimeSpent : Maybe Int
    , ccmRegistrationDate : Maybe String
    , dateOfService : Maybe String
    , billingDate : Maybe String
    , billingMonth : Int
    , billingYear : Int
    , isClosed : Bool
    , tocId : Maybe Int
    , readmission : Bool
    , isComplexCCM : Bool
    , batchCloseOnInvoiceCompletion : Bool
    , reviewedByStaffName : Maybe String
    , canModifyReviewedStatus : Bool
    , isReviewed : Bool
    , dxPresent : Bool
    , carePlanPresent : Bool
    , medsPresent : Bool
    , allergiesPresent : Bool
    , vitalsPresent : Bool
    , recordingPresent : Bool
    , chartComplete : Bool
    , status : Maybe String

    --, is24HoursSinceBilled : Bool
    }


view : Model -> Maybe AddEditDataSource -> Html Msg
view model addEditDataSource =
    Table.view model.gridOperations model.rows (gridConfig addEditDataSource) Nothing


type Msg
    = Load (Result Http.Error LoadResult)
    | SetGridOperations (Table.GridOperations Row)
    | Reset


update : Msg -> Model -> Int -> ( Model, Cmd Msg )
update msg model patientId =
    case msg of
        Load (Ok t) ->
            { model | rows = t.result, gridOperations = Table.updateFromServer t.filters t.serverData model.gridOperations } ! []

        Load (Err t) ->
            model ! [ Functions.displayErrorMessage (toString t) ]

        SetGridOperations gridOperations ->
            { model | gridOperations = gridOperations } ! [ load patientId gridOperations ]

        Reset ->
            model ! []



-- Paging stuff


decodeBillingCcm : Decode.Decoder Row
decodeBillingCcm =
    Pipeline.decode Row
        |> Pipeline.required "ID" (Decode.int)
        |> Pipeline.required "Facility" (Decode.maybe Decode.string)
        |> Pipeline.required "FacilityId" (Decode.int)
        |> Pipeline.required "PracticeLocation" (Decode.maybe Decode.string)
        |> Pipeline.required "MainProvider" (Decode.maybe Decode.string)
        |> Pipeline.required "ProviderId" (Decode.int)
        |> Pipeline.required "PatientName" (Decode.maybe Decode.string)
        |> Pipeline.required "PatientId" (Decode.int)
        |> Pipeline.required "DoB" (Decode.maybe Decode.string)
        |> Pipeline.required "PatientFacilityIdNo" (Decode.maybe Decode.string)
        |> Pipeline.required "Phone" (Decode.maybe Decode.string)
        |> Pipeline.required "AssignedTo" (Decode.maybe Decode.string)
        |> Pipeline.required "StaffId" (Decode.maybe Decode.int)
        |> Pipeline.required "OpenTasks" (Decode.int)
        |> Pipeline.required "TotalTimeSpent" (Decode.maybe Decode.int)
        |> Pipeline.required "CcmRegistrationDate" (Decode.maybe Decode.string)
        |> Pipeline.required "DateOfService" (Decode.maybe Decode.string)
        |> Pipeline.required "BillingDate" (Decode.maybe Decode.string)
        |> Pipeline.required "BillingMonth" (Decode.int)
        |> Pipeline.required "BillingYear" (Decode.int)
        |> Pipeline.required "IsClosed" (Decode.bool)
        |> Pipeline.required "TocId" (Decode.maybe Decode.int)
        |> Pipeline.required "Readmission" (Decode.bool)
        |> Pipeline.required "IsComplexCCM" (Decode.bool)
        |> Pipeline.required "BatchCloseOnInvoiceCompletion" (Decode.bool)
        |> Pipeline.required "ReviewedByStaffName" (Decode.maybe Decode.string)
        |> Pipeline.required "CanModifyReviewedStatus" (Decode.bool)
        |> Pipeline.required "IsReviewed" (Decode.bool)
        |> Pipeline.required "DxPresent" (Decode.bool)
        |> Pipeline.required "CarePlanPresent" (Decode.bool)
        |> Pipeline.required "MedsPresent" (Decode.bool)
        |> Pipeline.required "AllergiesPresent" (Decode.bool)
        |> Pipeline.required "VitalsPresent" (Decode.bool)
        |> Pipeline.required "RecordingPresent" Decode.bool
        |> Pipeline.required "ChartComplete" (Decode.bool)
        |> Pipeline.required "Status" (Decode.maybe Decode.string)



-- |> Pipeline.required "Is24HoursSinceBilled" (Decode.bool)


type alias LoadResult =
    { result : List Row
    , serverData : Table.ServerData
    , filters : Row
    }


jsonDecodeLoad : Decode.Decoder LoadResult
jsonDecodeLoad =
    Pipeline.decode LoadResult
        |> Pipeline.required "Data" (Decode.list decodeBillingCcm)
        |> Pipeline.required "GridOperations" Table.decodeGridOperations
        |> Pipeline.required "Filters" decodeBillingCcm


encodeEditData : Row -> Encode.Value
encodeEditData newRecord =
    Encode.object
        [ ( "ID", Encode.int <| newRecord.id )
        , ( "Facility", (maybeVal Encode.string) <| newRecord.facility )
        , ( "FacilityId", Encode.int <| newRecord.facilityId )
        , ( "PracticeLocation", (maybeVal Encode.string) <| newRecord.practiceLocation )
        , ( "MainProvider", (maybeVal Encode.string) <| newRecord.mainProvider )
        , ( "ProviderId", (Encode.int) <| newRecord.providerId )
        , ( "PatientName", (maybeVal Encode.string) <| newRecord.patientName )
        , ( "PatientId", (Encode.int) <| newRecord.patientId )
        , ( "DoB", (maybeVal Encode.string) <| newRecord.dob )
        , ( "PatientFacilityIdNo", (maybeVal Encode.string) <| newRecord.patientFacilityIdNo )
        , ( "Phone", (maybeVal Encode.string) <| newRecord.phone )
        , ( "AssignedTo", (maybeVal Encode.string) <| newRecord.assignedTo )
        , ( "StaffId", (maybeVal Encode.int) <| newRecord.staffId )
        , ( "OpenTasks", (Encode.int) <| newRecord.openTasks )
        , ( "TotalTimeSpent", (maybeVal Encode.int) <| newRecord.totalTimeSpent )
        , ( "CcmRegistrationDate", (maybeVal Encode.string) <| newRecord.ccmRegistrationDate )
        , ( "DateOfService", (maybeVal Encode.string) <| newRecord.dateOfService )
        , ( "BillingDate", (maybeVal Encode.string) <| newRecord.billingDate )
        , ( "BillingMonth", (Encode.int) <| newRecord.billingMonth )
        , ( "BillingYear", (Encode.int) <| newRecord.billingYear )
        , ( "IsClosed", (Encode.bool) <| newRecord.isClosed )
        , ( "TocId", (maybeVal Encode.int) <| newRecord.tocId )
        , ( "Readmission", (Encode.bool) <| newRecord.readmission )
        , ( "IsComplexCCM", (Encode.bool) <| newRecord.isComplexCCM )
        , ( "BatchCloseOnInvoiceCompletion", (Encode.bool) <| newRecord.batchCloseOnInvoiceCompletion )
        , ( "ReviewedByStaffName", (maybeVal Encode.string) <| newRecord.reviewedByStaffName )
        , ( "CanModifyReviewedStatus", (Encode.bool) <| newRecord.canModifyReviewedStatus )
        , ( "IsReviewed", (Encode.bool) <| newRecord.isReviewed )
        , ( "DxPresent", (Encode.bool) <| newRecord.dxPresent )
        , ( "CarePlanPresent", (Encode.bool) <| newRecord.carePlanPresent )
        , ( "MedsPresent", (Encode.bool) <| newRecord.medsPresent )
        , ( "AllergiesPresent", (Encode.bool) <| newRecord.allergiesPresent )
        , ( "VitalsPresent", (Encode.bool) <| newRecord.vitalsPresent )
        , ( "RecordingPresent", Encode.bool <| newRecord.recordingPresent )
        , ( "ChartComplete", (Encode.bool) <| newRecord.chartComplete )
        , ( "Status", (maybeVal Encode.string) <| newRecord.status )
        ]


load : Int -> Table.GridOperations Row -> Cmd Msg
load patientId gridOperations =
    Http.request
        { body =
            Encode.object
                [ ( "patientId", Encode.int <| patientId )
                , ( "gridOperations", Table.encodeGridOperations gridOperations )
                , ( "filters", encodeEditData gridOperations.filters )
                ]
                |> Http.jsonBody
        , expect = Http.expectJson jsonDecodeLoad
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = "/People/BillingTest"
        , withCredentials = False
        }
        |> Http.send Load


emptyModel : Model
emptyModel =
    { rows = []
    , gridOperations = Table.init emptyRow "Date"
    , query = ""
    }


gridConfig : Maybe AddEditDataSource -> Table.Config Row Msg
gridConfig addEditDataSource =
    { domTableId = "RecordTable"
    , toolbar = []
    , toMsg = SetGridOperations
    , columns = getColumns addEditDataSource
    }


getColumns : Maybe AddEditDataSource -> List (Table.Column Row Msg)
getColumns addEditDataSource =
    [ --checkColumn "" ,
      stringColumn "Facility" .facility "Facility"

    -- , stringColumn "Billing Date" (\t -> Functions.dateFormat "MMMM YYYY" t.billingDate)
    , stringColumn "Main Provider" .mainProvider "MainProvider"
    , stringColumn "Patient Name" .patientName "PatientName"
    , dateColumn "DOB" .dob "DOB"
    , stringColumn "Id No" .patientFacilityIdNo "PatientFacilityIdNo"
    , stringColumn "AssignedTo" .assignedTo "AssignedTo"
    ]


emptyRow : Row
emptyRow =
    { id = 0
    , facility = Nothing
    , facilityId = 0
    , practiceLocation = Nothing
    , mainProvider = Nothing
    , providerId = 0
    , patientName = Nothing
    , patientId = 0
    , dob = Nothing
    , patientFacilityIdNo = Nothing
    , phone = Nothing
    , assignedTo = Nothing
    , staffId = Nothing
    , openTasks = 0
    , totalTimeSpent = Nothing
    , ccmRegistrationDate = Nothing
    , dateOfService = Nothing
    , billingDate = Nothing
    , billingMonth = 0
    , billingYear = 0
    , isClosed = False
    , tocId = Nothing
    , readmission = False
    , isComplexCCM = False
    , batchCloseOnInvoiceCompletion = False
    , reviewedByStaffName = Nothing
    , canModifyReviewedStatus = False
    , isReviewed = False
    , dxPresent = False
    , carePlanPresent = False
    , medsPresent = False
    , allergiesPresent = False
    , vitalsPresent = False
    , recordingPresent = False
    , chartComplete = False
    , status = Nothing

    --, is24HoursSinceBilled = False
    }
