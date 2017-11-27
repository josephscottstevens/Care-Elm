port module Records.Main exposing (..)

import Records.Functions exposing (..)
import Records.Types exposing (..)
import Html exposing (Html, text, div, button)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Table exposing (..)
import Common.Grid exposing (..)
import Common.Types exposing (..)
import Common.Functions exposing (..)


port sendMenuMessage : MenuMessage -> Cmd msg


port toggleConsent : Bool -> Cmd msg


port editTask : Int -> Cmd msg


port dropDownToggle : (Int -> msg) -> Sub msg


port deleteConfirmed : (Int -> msg) -> Sub msg


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ dropDownToggle DropDownToggle
        , deleteConfirmed DeleteConfirmed
        ]


init : Flags -> Cmd Msg
init flags =
    getRecords flags.patientId flags.recordTypeId Load


update : Msg -> Model -> ( ( Model, Cmd Msg ), Maybe AddEditDataSource )
update msg model =
    case msg of
        Load (Ok t) ->
            ( getLoadedState model t ! [ setLoadingStatus False ], Nothing )

        Load (Err httpError) ->
            ( { model | state = Error (toString httpError) } ! [ setLoadingStatus False ], Nothing )

        SetTableState newState ->
            ( { model | tableState = newState } ! [], Nothing )

        SendMenuMessage recordId messageType ->
            ( { model | records = flipConsent model.records recordId model.recordTypeId } ! [ sendMenuMessage (getMenuMessage model recordId messageType) ], Nothing )

        DropDownToggle recordId ->
            ( { model | records = flipDropDownOpen model.records recordId } ! [], Nothing )

        DeleteConfirmed rowId ->
            let
                updatedRecords =
                    model.records |> List.filter (\t -> t.id /= rowId)
            in
                ( { model | records = updatedRecords } ! [ deleteRequest rowId ], Nothing )

        DeleteCompleted (Ok responseMsg) ->
            case getResponseError responseMsg of
                Just t ->
                    ( model ! [ displayErrorMessage t ], Nothing )

                Nothing ->
                    ( model ! [ displaySuccessMessage "Record deleted successfully!" ], Nothing )

        DeleteCompleted (Err httpError) ->
            ( { model | state = Error (toString httpError) } ! [], Nothing )

        EditTask taskId ->
            ( model ! [ editTask taskId ], Nothing )

        SetFilter filterState ->
            ( { model | filterFields = filterFields model.filterFields filterState } ! [], Nothing )

        AddNewStart addEditDataSource ->
            ( model ! [], Just addEditDataSource )


view : Model -> Maybe AddEditDataSource -> Html Msg
view model addEditDataSource =
    case model.state of
        Grid ->
            div []
                [ case addEditDataSource of
                    Just t ->
                        button [ type_ "button", class "btn btn-sm btn-default margin-bottom-5", onClick (AddNewStart t) ] [ text "New Record" ]

                    Nothing ->
                        button [ type_ "button", class "btn btn-sm btn-default margin-bottom-5 disabled" ] [ text "New Record" ]
                , div [ class "e-grid e-js e-waitingpopup" ]
                    [ Table.view (config SetFilter model.recordTypeId (updateTaskId model)) model.tableState (filteredRecords model) ]
                ]

        Limbo ->
            div [] []

        Error errMessage ->
            div [] [ text errMessage ]


getColumns : Maybe Int -> Maybe Int -> List (Column RecordRow Msg)
getColumns recordTypeId taskId =
    let
        commonColumns =
            [ stringColumn "Date Collected" (\t -> defaultDateTime t.date)
            , stringColumn "Doctor of Visit" (\t -> defaultString t.provider)
            , stringColumn "Specialty" (\t -> defaultString t.specialty)
            , stringColumn "Comments" (\t -> defaultString t.comments)
            ]

        firstColumns =
            case getRecordType recordTypeId of
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
            [ rowDropDownColumn recordTypeId
            ]
    in
        List.append firstColumns lastColumns


rowDropDownColumn : Maybe Int -> Table.Column RecordRow Msg
rowDropDownColumn recordTypeId =
    Table.veryCustomColumn
        { name = ""
        , viewData = \t -> rowDropDownDiv t.dropDownOpen (onClick (DropDownToggle t.id)) (dropDownItems recordTypeId t.id)
        , sorter = Table.unsortable
        }


config : (FilterState -> Msg) -> Maybe Int -> Maybe Int -> Config RecordRow Msg
config msg recordTypeId taskId =
    customConfig
        { toId = \t -> toString t.id
        , toMsg = SetTableState
        , columns = getColumns recordTypeId taskId
        , customizations =
            { defaultCustomizations | tableAttrs = standardTableAttrs "RecordTable", thead = standardThead msg }
        }


dropDownItems : Maybe Int -> Int -> List ( String, String, Html.Attribute Msg )
dropDownItems recordTypeId rowId =
    case getRecordType recordTypeId of
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
