port module Records.Main exposing (..)

import Records.Functions exposing (..)
import Records.Types exposing (..)
import Html exposing (Html, text, div, button, h4)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Table exposing (..)
import Common.Grid exposing (..)
import Common.Types exposing (..)
import Common.Functions exposing (..)
import Ports exposing (..)


port sendMenuMessage : MenuMessage -> Cmd msg


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ dropDownToggle DropDownToggle
        , deleteConfirmed DeleteConfirmed
        ]


init : RecordType -> Int -> Cmd Msg
init recordType patientId =
    let
        recordTypeId =
            getId recordType
    in
        getRecords patientId recordTypeId Load


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load (Ok t) ->
            getLoadedState model t ! [ setLoadingStatus False ]

        Load (Err t) ->
            model ! [ displayErrorMessage (toString t) ]

        SetTableState newState ->
            { model | tableState = newState } ! []

        SendMenuMessage recordId messageType ->
            { model | records = flipConsent model.records recordId model.recordType }
                ! [ sendMenuMessage (getMenuMessage model recordId messageType) ]

        DropDownToggle recordId ->
            { model | records = flipDropDownOpen model.records recordId } ! []

        DeleteConfirmed rowId ->
            let
                updatedRecords =
                    model.records |> List.filter (\t -> t.id /= rowId)
            in
                { model | records = updatedRecords } ! [ deleteRequest rowId ]

        DeleteCompleted (Ok responseMsg) ->
            case getResponseError responseMsg of
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

        AddNewStart ->
            (model ! [])


view : Model -> Maybe AddEditDataSource -> Html Msg
view model addEditDataSource =
    div []
        [ h4 [] [ text (toString model.recordType ++ " Records") ]
        , case addEditDataSource of
            Just _ ->
                button [ type_ "button", class "btn btn-sm btn-default margin-bottom-5", onClick AddNewStart ] [ text "New Record" ]

            Nothing ->
                button [ type_ "button", class "btn btn-sm btn-default margin-bottom-5 disabled" ] [ text "New Record" ]
        , div [ class "e-grid e-js e-waitingpopup" ]
            [ Table.view (config SetFilter model.recordType (updateTaskId model)) model.tableState (filteredRecords model) ]
        ]


getColumns : RecordType -> Maybe Int -> List (Column RecordRow Msg)
getColumns recordType taskId =
    let
        commonColumns =
            [ stringColumn "Date Collected" (\t -> defaultDateTime t.date)
            , stringColumn "Doctor of Visit" (\t -> defaultString t.provider)
            , stringColumn "Specialty" (\t -> defaultString t.specialty)
            , stringColumn "Comments" (\t -> defaultString t.comments)
            ]

        firstColumns =
            case recordType of
                PrimaryCare ->
                    commonColumns

                Specialty ->
                    commonColumns

                Labs ->
                    [ stringColumn "Date Collected" (\t -> defaultDateTime t.date)
                    , stringColumn "Date Accessioned" (\t -> defaultDateTime t.dateAccessed)
                    , stringColumn "Name of Lab" (\t -> defaultString t.title)
                    , stringColumn "Provider" (\t -> defaultString t.provider)
                    , stringColumn "Comments" (\t -> defaultString t.comments)
                    ]

                Radiology ->
                    [ stringColumn "Date Collected" (\t -> defaultDateTime t.date)
                    , stringColumn "Date Accessioned" (\t -> defaultDateTime t.dateAccessed)
                    , stringColumn "Name of Study" (\t -> defaultString t.title)
                    , stringColumn "Provider" (\t -> defaultString t.provider)
                    , stringColumn "Comments" (\t -> defaultString t.comments)
                    ]

                Hospitalizations ->
                    [ stringColumn "Date Collected" (\t -> defaultDateTime t.date)
                    , stringColumn "Hospitalization ID" (\t -> defaultIntToString t.hospitalizationId)
                    , stringColumn "Admin Date" (\t -> defaultDateTime t.dateOfAdmission)
                    , stringColumn "Discharge Date" (\t -> defaultDateTime t.dateOfDischarge)
                    , stringColumn "Service Type" (\t -> defaultString t.hospitalizationServiceType)
                    , stringColumn "Discharge Recommendations" (\t -> defaultString t.recommendations)
                    , stringColumn "Discharge Physician" (\t -> defaultString t.dischargePhysician)
                    , stringColumn "Comments" (\t -> defaultString t.comments)
                    ]

                Legal ->
                    [ stringColumn "Date Collected" (\t -> defaultDateTime t.date)
                    , stringColumn "Comments" (\t -> defaultString t.comments)
                    ]

                CallRecordings ->
                    [ stringColumn "Date" (\t -> dateTime t.recordingDate)
                    , hrefColumn "Recording" "Open" (\t -> defaultString t.recording)
                    , hrefColumnExtra "Task" (\t -> defaultString t.taskTitle) "#" (EditTask (defaultInt taskId))
                    , checkColumn "During Enrollment" (\t -> t.enrollment)
                    , checkColumn "Consent" (\t -> t.hasVerbalConsent)
                    , stringColumn "User" (\t -> defaultString t.staffName)
                    ]

                PreviousHistories ->
                    [ stringColumn "Date Collected" (\t -> defaultDateTime t.date)
                    , stringColumn "File Name" (\t -> defaultString t.fileName)
                    , stringColumn "Report Date" (\t -> defaultDate t.reportDate)
                    , stringColumn "Comments" (\t -> defaultString t.comments)
                    ]

                Enrollment ->
                    [ stringColumn "Date Collected" (\t -> defaultDateTime t.date)
                    , stringColumn "Comments" (\t -> defaultString t.comments)
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
        , viewData = \t -> rowDropDownDiv t.dropDownOpen (onClick (DropDownToggle t.id)) (dropDownItems recordType t.id)
        , sorter = Table.unsortable
        }


config : (FilterState -> Msg) -> RecordType -> Maybe Int -> Config RecordRow Msg
config event recordType taskId =
    customConfig
        { toId = \t -> toString t.id
        , toMsg = SetTableState
        , columns = getColumns recordType taskId
        , customizations =
            { defaultCustomizations | tableAttrs = standardTableAttrs "RecordTable", thead = standardThead event }
        }


dropDownItems : RecordType -> Int -> List ( String, String, Html.Attribute Msg )
dropDownItems recordType rowId =
    case recordType of
        CallRecordings ->
            [ ( "e-edit", "Mark As Consent", onClick (SendMenuMessage rowId "MarkAsConsent") ) ]

        _ ->
            [ ( "e-sync", "Transfer", onClick (SendMenuMessage rowId "Transfer") )
            , ( "e-download", "View File", onClick (SendMenuMessage rowId "ViewFile") )
            , ( "e-mail", "Send By Email", onClick (SendMenuMessage rowId "SendByEmail") )
            , ( "e-print_01", "Send By Fax", onClick (SendMenuMessage rowId "SendByFax") )
            , ( "e-save", "Save To Client Portal", onClick (SendMenuMessage rowId "SaveToClientPortal") )
            , ( "e-contextdelete", "Delete", onClick (SendMenuMessage rowId "Delete") )
            ]
