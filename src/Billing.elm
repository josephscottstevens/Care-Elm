module Billing exposing (Msg, Model, emptyModel, subscriptions, init, update, view)

import Html exposing (Html)
import Common.ServerTable as Table
import Common.Functions as Functions
import Common.Types exposing (AddEditDataSource)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


init : Int -> Cmd Msg
init patientId =
    Cmd.batch
        [ Table.initFilter columns
        , load patientId <| Table.init gridConfig
        ]


subscriptions : Sub Msg
subscriptions =
    Table.updateFilters UpdateFilters


type alias Model =
    { rows : List Row
    , gridOperations : Table.GridOperations Row Msg
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
    , is24HoursSinceBilled : Bool
    }


view : Model -> Maybe AddEditDataSource -> Html Msg
view model _ =
    Table.view model.gridOperations SetGridOperations model.rows Nothing


columns : List (Table.Column Row Msg)
columns =
    [ Table.htmlColumn "<= 24 Hrs"
        (\t ->
            if t.is24HoursSinceBilled then
                Just "NEW"
            else
                Nothing
        )
        "Is24HoursSinceBilled"
        "FILTER_IS_NEW"
    , Table.checkColumn "Reviewed" .isReviewed "IsReviewed"
    , Table.checkColumn "Batch Close" .batchCloseOnInvoiceCompletion "BatchCloseOnInvoiceCompletion"
    , Table.stringColumn "Facility" .facility "Facility"
    , Table.stringColumn "Billing Date" (\t -> Functions.formatDateTime "MMMM YYYY" t.billingDate) "BillingDate"
    , Table.stringColumn "Main Provider" .mainProvider "MainProvider"
    , Table.stringColumn "Patient Name" .patientName "PatientName"
    , Table.dateColumn "DOB" .dob "DoB"
    , Table.stringColumn "Id No" .patientFacilityIdNo "PatientFacilityIdNo"
    , Table.stringColumn "AssignedTo" .assignedTo "AssignedTo"
    ]


type Msg
    = Load (Result Http.Error LoadResult)
    | SetGridOperations (Table.GridOperations Row Msg)
    | UpdateFilters (List Table.Filter)


update : Msg -> Model -> Int -> ( Model, Cmd Msg )
update msg model patientId =
    case msg of
        Load (Ok t) ->
            { model | rows = t.result, gridOperations = Table.updateFromServer t.serverData model.gridOperations }
                ! []

        Load (Err t) ->
            model ! [ Functions.displayErrorMessage (toString t) ]

        SetGridOperations gridOperations ->
            { model | gridOperations = gridOperations }
                ! [ load patientId gridOperations ]

        UpdateFilters filters ->
            let
                gridOperations =
                    Table.updateFilter filters model.gridOperations
            in
                { model | gridOperations = gridOperations }
                    ! [ load patientId gridOperations ]



-- Paging stuff


decodeBillingCcm : Decode.Decoder Row
decodeBillingCcm =
    Pipeline.decode Row
        |> Pipeline.required "ID" Decode.int
        |> Pipeline.required "Facility" (Decode.maybe Decode.string)
        |> Pipeline.required "FacilityId" Decode.int
        |> Pipeline.required "PracticeLocation" (Decode.maybe Decode.string)
        |> Pipeline.required "MainProvider" (Decode.maybe Decode.string)
        |> Pipeline.required "ProviderId" Decode.int
        |> Pipeline.required "PatientName" (Decode.maybe Decode.string)
        |> Pipeline.required "PatientId" Decode.int
        |> Pipeline.required "DoB" (Decode.maybe Decode.string)
        |> Pipeline.required "PatientFacilityIdNo" (Decode.maybe Decode.string)
        |> Pipeline.required "Phone" (Decode.maybe Decode.string)
        |> Pipeline.required "AssignedTo" (Decode.maybe Decode.string)
        |> Pipeline.required "StaffId" (Decode.maybe Decode.int)
        |> Pipeline.required "OpenTasks" Decode.int
        |> Pipeline.required "TotalTimeSpent" (Decode.maybe Decode.int)
        |> Pipeline.required "CcmRegistrationDate" (Decode.maybe Decode.string)
        |> Pipeline.required "DateOfService" (Decode.maybe Decode.string)
        |> Pipeline.required "BillingDate" (Decode.maybe Decode.string)
        |> Pipeline.required "BillingMonth" Decode.int
        |> Pipeline.required "BillingYear" Decode.int
        |> Pipeline.required "IsClosed" Decode.bool
        |> Pipeline.required "TocId" (Decode.maybe Decode.int)
        |> Pipeline.required "Readmission" Decode.bool
        |> Pipeline.required "IsComplexCCM" Decode.bool
        |> Pipeline.required "BatchCloseOnInvoiceCompletion" Decode.bool
        |> Pipeline.required "ReviewedByStaffName" (Decode.maybe Decode.string)
        |> Pipeline.required "CanModifyReviewedStatus" Decode.bool
        |> Pipeline.required "IsReviewed" Decode.bool
        |> Pipeline.required "DxPresent" Decode.bool
        |> Pipeline.required "CarePlanPresent" Decode.bool
        |> Pipeline.required "MedsPresent" Decode.bool
        |> Pipeline.required "AllergiesPresent" Decode.bool
        |> Pipeline.required "VitalsPresent" Decode.bool
        |> Pipeline.required "RecordingPresent" Decode.bool
        |> Pipeline.required "ChartComplete" Decode.bool
        |> Pipeline.required "Status" (Decode.maybe Decode.string)
        |> Pipeline.required "Is24HoursSinceBilled" Decode.bool



-- |> Pipeline.required "Is24HoursSinceBilled" (Decode.bool)


type alias LoadResult =
    { result : List Row
    , serverData : Table.ServerData
    }


jsonDecodeLoad : Decode.Decoder LoadResult
jsonDecodeLoad =
    Pipeline.decode LoadResult
        |> Pipeline.required "Data" (Decode.list decodeBillingCcm)
        |> Pipeline.required "GridOperations" Table.decodeGridOperations


load : Int -> Table.GridOperations Row Msg -> Cmd Msg
load patientId gridOperations =
    Http.request
        { body =
            Encode.object
                [ ( "patientId", Encode.int patientId )
                , ( "gridOperations", Table.encodeGridOperations gridOperations )
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
    , gridOperations = Table.init gridConfig
    }


gridConfig : Table.Config Row Msg
gridConfig =
    { domTableId = "BillingTable"
    , sortField = Just "DOB"
    , toolbar = []
    , columns = columns
    }
