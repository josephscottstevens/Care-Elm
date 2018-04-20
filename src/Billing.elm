module Billing exposing (Msg, Model, emptyModel, subscriptions, init, update, view)

import Html exposing (Html, div, text, input)
import Html.Attributes exposing (class, title, style, checked, type_)
import Html.Events exposing (onClick)
import Common.ServerTable as Table exposing (ColumnStyle(Width, CustomStyle), Operator(..))
import Common.Functions as Functions
import Common.Types exposing (AddEditDataSource)
import Common.Html exposing (ConfirmDialog, viewConfirm)
import Http
import Task
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


init : Int -> Cmd Msg
init patientId =
    Cmd.batch
        [ load patientId <| Table.init gridConfig
        , Table.initFilter columns
        ]


subscriptions : Sub Msg
subscriptions =
    Table.updateFilters UpdateFilters


type alias Model =
    { rows : List Row
    , gridOperations : Table.GridOperations Row Msg
    , confirmData : Maybe (ConfirmDialog Row)
    }


type alias Row =
    { id : Int
    , facility : Maybe String
    , facilityId : Int
    , mainProvider : Maybe String
    , patientName : Maybe String
    , patientId : Int
    , dob : Maybe String
    , patientFacilityIdNo : Maybe String
    , assignedTo : Maybe String
    , openTasks : Int
    , totalTimeSpent : Maybe Int
    , ccmRegistrationDate : Maybe String
    , billingDate : Maybe String
    , isClosed : Bool
    , batchCloseOnInvoiceCompletion : Bool
    , isReviewed : Bool
    , dxPresent : Bool
    , carePlanPresent : Bool
    , medsPresent : Bool
    , allergiesPresent : Bool
    , vitalsPresent : Bool
    , recordingPresent : Bool
    , chartComplete : Bool
    , is24HoursSinceBilled : Bool
    , billingCode : Maybe String
    }


view : Model -> Maybe AddEditDataSource -> Html Msg
view model _ =
    div []
        [ Table.view model.gridOperations SetGridOperations model.rows Nothing
        , viewConfirm model.confirmData Test Test2
        ]


columns : List (Table.Column Row Msg)
columns =
    let
        checkHelper t =
            div [ class "e-checkcell" ]
                [ div [ class "e-checkcelldiv", style [ ( "text-align", "center" ) ] ]
                    [ input (type_ "checkbox" :: t) []
                    ]
                ]

        batchClose row =
            checkHelper
                [ checked row.batchCloseOnInvoiceCompletion
                , onClick (ToggleBatchClose row)
                ]

        toggleReviewed row =
            checkHelper
                [ checked row.isReviewed
                , if row.isReviewed == True then
                    onClick (ToggleReviewed row)
                  else
                    onClick (ConfirmDialogShow row.id)
                ]

        dxCpRcAlRxVsOperator =
            CustomSingleOperator "Equals" [ "DxPresent", "CarePlanPresent", "RecordingPresent", "AllergiesPresent", "MedsPresent", "VitalsPresent" ]

        isNew t =
            if t.is24HoursSinceBilled then
                text "NEW"
            else
                text ""

        billingDate t =
            t.billingDate
                |> Functions.formatDateTime "MMMM YYYY"
                |> Maybe.withDefault ""
                |> text

        timeSpent t =
            t.totalTimeSpent
                |> Maybe.map Functions.ticksToSeconds
                |> Maybe.map Functions.secondsToHHMMSS

        openTasks t =
            Just (toString t.openTasks ++ " Tasks")

        filterStyle =
            CustomStyle
                [ ( "width", "100px" )
                , ( "padding-left", "10px" )
                ]
    in
        [ Table.htmlColumn "<= 24 Hrs" "lessThan24HoursColumn" (Width 4) isNew Table.FilterIsNewControl (Equals "Is24HoursSinceBilled")
        , Table.htmlColumn "Reviewed" "reviewedColumn" (Width 4) toggleReviewed Table.CheckBoxControl (Equals "IsReviewed")
        , Table.htmlColumn "Batch Close" "batchCloseColumn" (Width 4) batchClose Table.CheckBoxControl (Equals "BatchCloseOnInvoiceCompletion")
        , Table.stringColumn "Facility" (Width 9) .facility "Facility"
        , Table.htmlColumn "Billing Date" "billingDateColumn" (Width 5) billingDate Table.Last60MonthsControl (Between "BillingDate" "BillingDate")
        , Table.stringColumn "Main Provider" (Width 5) .mainProvider "MainProvider"
        , Table.stringColumn "Patient Name" (Width 5) .patientName "PatientName"
        , Table.dateColumn "DOB" (Width 5) .dob "DoB"
        , Table.stringColumn "Patient's Facility Id" (Width 5) .patientFacilityIdNo "PatientFacilityIdNo"
        , Table.stringColumn "AssignedTo" (Width 5) .assignedTo "AssignedTo"
        , Table.stringColumn "Time Spent" (Width 4) timeSpent "TotalTimeSpent"
        , Table.hrefColumn "Open Tasks" (Width 3) openTasks (\t -> toString t.openTasks ++ " Tasks") "OpenTasks"
        , Table.dateColumn "CCM Enrollment" (Width 5) .ccmRegistrationDate "CcmRegistrationDate"
        , Table.stringColumn "Billing Codes" (Width 3) .billingCode "BillingCode"
        , Table.htmlColumn "Dx CP RC Al Rx VS" "presentFlagsColumn" filterStyle dxCpRcAlRxVs Table.SixCirclesControl dxCpRcAlRxVsOperator
        , Table.dropdownColumn (Width 2)
            [ ( "", "Generate Summary Report", GenerateSummaryReport )
            , ( "", "Save Summary Report to Client Portal", SaveSummaryReportToClientPortal )
            , ( "", "Close Billing Session", CloseBillingSession )
            ]
        ]


dxCpRcAlRxVs : Row -> Html msg
dxCpRcAlRxVs row =
    let
        getStyle t =
            if t then
                style [ ( "background-color", "currentColor" ) ]
            else
                style [ ( "background-color", "white" ), ( "border", "1.4px solid" ) ]

        fullCircle ( t, titleText ) =
            div [ title titleText, class "circle", getStyle t ] []
    in
        [ ( row.dxPresent, "Chronic Diagnoses present?" )
        , ( row.carePlanPresent, "Care Plan present?" )
        , ( row.recordingPresent, "Recorded call present?" )
        , ( row.allergiesPresent, "Allergies present?" )
        , ( row.medsPresent, "Medication present?" )
        , ( row.vitalsPresent, "Vitals Signs present?" )
        ]
            |> List.map fullCircle
            |> div [ class "circleMargin" ]


type Msg
    = Load (Result Http.Error LoadResult)
    | SetGridOperations (Table.GridOperations Row Msg)
    | UpdateFilters (List Table.Filter)
    | GenerateSummaryReport Row
    | SaveSummaryReportToClientPortal Row
    | CloseBillingSession Row
    | ToggleBatchClose Row
    | ToggleBatchCloseDone (Result Http.Error String)
    | ToggleReviewed Row
    | ToggleReviewedDone (Result Http.Error String)
    | ConfirmDialogShow Int
    | ConfirmDialogConfirmed Int
    | Test
    | Test2 Row


toggleReviewed : Row -> Cmd Msg
toggleReviewed row =
    "/Billing/ToggleBillingRecordReviewed"
        |> Functions.postRequest
            (Encode.object [ ( "billingId", Encode.int row.id ) ])
        |> Http.send ToggleReviewedDone


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

        GenerateSummaryReport row ->
            Debug.crash "todo"

        SaveSummaryReportToClientPortal row ->
            { model
                | confirmData =
                    Just
                        { data = row
                        , headerText = "Save to Client Portal"
                        , message = "Are you sure that you want to save this report in Clinical Portal?"
                        }
            }
                ! []

        CloseBillingSession row ->
            Debug.crash "todo"

        ToggleBatchClose row ->
            model
                ! [ "/Billing/ToggleBatchInvoice"
                        |> Functions.postRequest
                            (Encode.object [ ( "billingId", Encode.int row.id ) ])
                        |> Http.send ToggleBatchCloseDone
                  ]

        ToggleBatchCloseDone (Ok t) ->
            model ! []

        ToggleBatchCloseDone (Err t) ->
            model ! [ Functions.displayErrorMessage (toString t) ]

        ToggleReviewed row ->
            model ! [ toggleReviewed row ]

        ToggleReviewedDone (Ok t) ->
            model ! []

        ToggleReviewedDone (Err t) ->
            model ! [ Functions.displayErrorMessage (toString t) ]

        ConfirmDialogShow id ->
            model
                ! [ Functions.customDialogShow
                        { message = "Are you sure you wish to change the reviewed status?"
                        , submitText = "Ok"
                        , title = "Confirm"
                        , id = id
                        }
                  ]

        ConfirmDialogConfirmed id ->
            let
                row =
                    model.rows
                        |> List.filter (\t -> t.id == id)
                        |> List.head
            in
                model
                    ! [ case row of
                            Just t ->
                                toggleReviewed t

                            Nothing ->
                                Functions.displayErrorMessage "Error toggling reviewed status, please try again later"
                      ]

        Test ->
            { model | confirmData = Nothing } ! []

        Test2 row ->
            { model | confirmData = Nothing } ! []



-- Paging stuff


decodeBillingCcm : Decode.Decoder Row
decodeBillingCcm =
    Pipeline.decode Row
        |> Pipeline.required "ID" Decode.int
        |> Pipeline.required "Facility" (Decode.maybe Decode.string)
        |> Pipeline.required "FacilityId" Decode.int
        |> Pipeline.required "MainProvider" (Decode.maybe Decode.string)
        |> Pipeline.required "PatientName" (Decode.maybe Decode.string)
        |> Pipeline.required "PatientId" Decode.int
        |> Pipeline.required "DoB" (Decode.maybe Decode.string)
        |> Pipeline.required "PatientFacilityIdNo" (Decode.maybe Decode.string)
        |> Pipeline.required "AssignedTo" (Decode.maybe Decode.string)
        |> Pipeline.required "OpenTasks" Decode.int
        |> Pipeline.required "TotalTimeSpent" (Decode.maybe Decode.int)
        |> Pipeline.required "CcmRegistrationDate" (Decode.maybe Decode.string)
        |> Pipeline.required "BillingDate" (Decode.maybe Decode.string)
        |> Pipeline.required "IsClosed" Decode.bool
        |> Pipeline.required "BatchCloseOnInvoiceCompletion" Decode.bool
        |> Pipeline.required "IsReviewed" Decode.bool
        |> Pipeline.required "DxPresent" Decode.bool
        |> Pipeline.required "CarePlanPresent" Decode.bool
        |> Pipeline.required "MedsPresent" Decode.bool
        |> Pipeline.required "AllergiesPresent" Decode.bool
        |> Pipeline.required "VitalsPresent" Decode.bool
        |> Pipeline.required "RecordingPresent" Decode.bool
        |> Pipeline.required "ChartComplete" Decode.bool
        |> Pipeline.required "Is24HoursSinceBilled" Decode.bool
        |> Pipeline.required "BillingCode" (Decode.maybe Decode.string)


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
    , confirmData = Nothing
    }


gridConfig : Table.Config Row Msg
gridConfig =
    { domTableId = "BillingTable"
    , sortField = Just "BillingDate"
    , rowsPerPage = 20
    , toolbar =
        []
    , columns = columns
    }
