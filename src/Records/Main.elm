module Records.Main exposing (..)

import Records.Functions exposing (..)
import Records.Types exposing (..)
import Html exposing (Html, text, div, button, h4)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Table exposing (stringColumn, defaultCustomizations)
import Common.Grid exposing (hrefColumn, checkColumn)
import Common.Types exposing (RecordType(..), AddEditDataSource, getDesc, FilterState)
import Common.Functions as Functions exposing (displaySuccessMessage, displayErrorMessage)
import Http
import Ports exposing (dropDownToggle, deleteConfirmed, sendMenuMessage, editTask)
import Route


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ dropDownToggle DropDownToggle
        , deleteConfirmed DeleteConfirmed
        ]


init : RecordType -> Int -> Cmd Msg
init recordType patientId =
    getRecords recordType patientId
        |> Http.send Load


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load (Ok t) ->
            { model | records = t } ! [ Functions.setLoadingStatus False ]

        Load (Err t) ->
            model ! [ displayErrorMessage (toString t) ]

        NewRecord ->
            model ! [ Route.modifyUrl (Route.RecordAddNew model.recordType) ]

        SetTableState newState ->
            { model | tableState = newState } ! []

        SendMenuMessage recordId recordType messageType ->
            { model | records = flipConsent model.records recordId recordType }
                ! [ sendMenuMessage (getMenuMessage model.records recordType recordId messageType) ]

        DropDownToggle recordId ->
            { model | records = flipDropDownOpen model.records recordId } ! []

        DeleteConfirmed rowId ->
            let
                updatedRecords =
                    model.records |> List.filter (\t -> t.id /= rowId)
            in
                { model | records = updatedRecords } ! [ deleteRequest rowId ]

        DeleteCompleted (Ok responseMsg) ->
            case Functions.getResponseError responseMsg of
                Just t ->
                    model ! [ displayErrorMessage t ]

                Nothing ->
                    model ! [ displaySuccessMessage "Record deleted successfully!" ]

        DeleteCompleted (Err t) ->
            model ! [ displayErrorMessage (toString t) ]

        EditTask taskId ->
            model ! [ editTask taskId ]

        SetFilter filterState ->
            { model | filterFields = filterFields model.filterFields filterState } ! []


view : Model -> Maybe AddEditDataSource -> Html Msg
view model addEditDataSource =
    div []
        [ h4 [] [ text (getDesc model.recordType) ]
        , case addEditDataSource of
            Just _ ->
                button [ type_ "button", class "btn btn-sm btn-default margin-bottom-5", onClick NewRecord ] [ text "New Record" ]

            Nothing ->
                button [ type_ "button", class "btn btn-sm btn-default margin-bottom-5 disabled" ] [ text "New Record" ]
        , div [ class "e-grid e-js e-waitingpopup" ]
            [ Table.view (config SetFilter model.recordType) model.tableState (filteredRecords model.records model.filterFields model.recordType) ]
        ]


getColumns : RecordType -> List (Table.Column RecordRow Msg)
getColumns recordType =
    let
        commonColumns =
            [ stringColumn "Date Collected" (\t -> Functions.defaultDateTime t.date)
            , stringColumn "Doctor of Visit" (\t -> Functions.defaultString t.provider)
            , stringColumn "Specialty" (\t -> Functions.defaultString t.specialty)
            , stringColumn "Comments" (\t -> Functions.defaultString t.comments)
            ]

        firstColumns =
            case recordType of
                PrimaryCare ->
                    commonColumns

                Specialty ->
                    commonColumns

                Labs ->
                    [ stringColumn "Date Collected" (\t -> Functions.defaultDateTime t.date)
                    , stringColumn "Date Accessioned" (\t -> Functions.defaultDateTime t.dateAccessed)
                    , stringColumn "Name of Lab" (\t -> Functions.defaultString t.title)
                    , stringColumn "Provider" (\t -> Functions.defaultString t.provider)
                    , stringColumn "Comments" (\t -> Functions.defaultString t.comments)
                    ]

                Radiology ->
                    [ stringColumn "Date Collected" (\t -> Functions.defaultDateTime t.date)
                    , stringColumn "Date Accessioned" (\t -> Functions.defaultDateTime t.dateAccessed)
                    , stringColumn "Name of Study" (\t -> Functions.defaultString t.title)
                    , stringColumn "Provider" (\t -> Functions.defaultString t.provider)
                    , stringColumn "Comments" (\t -> Functions.defaultString t.comments)
                    ]

                Hospitalizations ->
                    [ stringColumn "Date Collected" (\t -> Functions.defaultDateTime t.date)
                    , stringColumn "Hospitalization ID" (\t -> Functions.defaultIntToString t.hospitalizationId)
                    , stringColumn "Admin Date" (\t -> Functions.defaultDateTime t.dateOfAdmission)
                    , stringColumn "Discharge Date" (\t -> Functions.defaultDateTime t.dateOfDischarge)
                    , stringColumn "Service Type" (\t -> Functions.defaultString t.hospitalizationServiceType)
                    , stringColumn "Discharge Recommendations" (\t -> Functions.defaultString t.recommendations)
                    , stringColumn "Discharge Physician" (\t -> Functions.defaultString t.dischargePhysician)
                    , stringColumn "Comments" (\t -> Functions.defaultString t.comments)
                    ]

                Legal ->
                    [ stringColumn "Date Collected" (\t -> Functions.defaultDateTime t.date)
                    , stringColumn "Comments" (\t -> Functions.defaultString t.comments)
                    ]

                CallRecordings ->
                    [ stringColumn "Date" (\t -> Functions.dateTime t.recordingDate)
                    , hrefColumn "Recording" "Open" (\t -> Functions.defaultString t.recording)
                    , hrefCustom
                    , checkColumn "During Enrollment" (\t -> t.enrollment)
                    , checkColumn "Consent" (\t -> t.hasVerbalConsent)
                    , stringColumn "User" (\t -> Functions.defaultString t.staffName)
                    ]

                PreviousHistories ->
                    [ stringColumn "Date Collected" (\t -> Functions.defaultDateTime t.date)
                    , stringColumn "File Name" (\t -> Functions.defaultString t.fileName)
                    , stringColumn "Report Date" (\t -> Functions.defaultDate t.reportDate)
                    , stringColumn "Comments" (\t -> Functions.defaultString t.comments)
                    ]

                Enrollment ->
                    [ stringColumn "Date Collected" (\t -> Functions.defaultDateTime t.date)
                    , stringColumn "Comments" (\t -> Functions.defaultString t.comments)
                    ]

                Misc ->
                    commonColumns

        lastColumns =
            [ rowDropDownColumn recordType
            ]
    in
        List.append firstColumns lastColumns


rowDropDownColumn : RecordType -> Table.Column RecordRow Msg
rowDropDownColumn recordType =
    Table.veryCustomColumn
        { name = ""
        , viewData = \t -> Common.Grid.rowDropDownDiv t.dropDownOpen (onClick (DropDownToggle t.id)) (dropDownItems recordType t.id)
        , sorter = Table.unsortable
        }


config : (FilterState -> Msg) -> RecordType -> Table.Config RecordRow Msg
config event recordType =
    Table.customConfig
        { toId = \t -> toString t.id
        , toMsg = SetTableState
        , columns = getColumns recordType
        , customizations =
            { defaultCustomizations
                | tableAttrs = Common.Grid.standardTableAttrs "RecordTable"
                , thead = Common.Grid.standardThead event
            }
        }


hrefCustom : Table.Column RecordRow Msg
hrefCustom =
    Table.veryCustomColumn
        { name = "Task"
        , viewData = \t -> hrefCustomDetails t.taskId t.taskTitle
        , sorter = Table.unsortable
        }


hrefCustomDetails : Maybe Int -> Maybe String -> Table.HtmlDetails Msg
hrefCustomDetails taskId taskTitle =
    Table.HtmlDetails []
        [ case ( taskId, taskTitle ) of
            ( Just t, Just y ) ->
                div [ class "RecordTableHref", onClick (EditTask t) ] [ text y ]

            _ ->
                div [] []
        ]


dropDownItems : RecordType -> Int -> List ( String, String, Html.Attribute Msg )
dropDownItems recordType rowId =
    case recordType of
        CallRecordings ->
            [ ( "e-edit", "Mark As Consent", onClick (SendMenuMessage rowId recordType "MarkAsConsent") ) ]

        _ ->
            [ ( "e-sync", "Transfer", onClick (SendMenuMessage rowId recordType "Transfer") )
            , ( "e-download", "View File", onClick (SendMenuMessage rowId recordType "ViewFile") )
            , ( "e-mail", "Send By Email", onClick (SendMenuMessage rowId recordType "SendByEmail") )
            , ( "e-print_01", "Send By Fax", onClick (SendMenuMessage rowId recordType "SendByFax") )
            , ( "e-save", "Save To Client Portal", onClick (SendMenuMessage rowId recordType "SaveToClientPortal") )
            , ( "e-contextdelete", "Delete", onClick (SendMenuMessage rowId recordType "Delete") )
            ]
