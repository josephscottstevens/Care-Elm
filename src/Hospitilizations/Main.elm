port module Hospitilizations.Main exposing (..)

import Hospitilizations.Functions exposing (getHospitilizations, getLoadedState, flipDropDownOpen, deleteHospitilization, filterFields, filteredRecords)
import Hospitilizations.Types exposing (..)
import Html exposing (Html, text, div, button, a)
import Html.Attributes exposing (class, type_, href)
import Html.Events exposing (onClick)
import Table exposing (defaultCustomizations)
import Common.Grid exposing (checkColumn, standardTableAttrs, standardThead, rowDropDownDiv)
import Common.Types exposing (MenuMessage, FilterState, AddEditDataSource, RecordAddNewInitData, HospitilizationsRow)
import Common.Functions exposing (setLoadingStatus, displayErrorMessage, getResponseError, displaySuccessMessage, defaultString, defaultDate)
import Ports exposing (dropDownToggle, sendMenuMessage)
import Common.Routes exposing (navHospitilizationsAddEdit)


port deleteHospitilizationConfirmed : (Int -> msg) -> Sub msg


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ deleteHospitilizationConfirmed DeleteHospitilizationConfirmed
        , dropDownToggle DropDownToggle
        ]


init : Int -> Cmd Msg
init patientId =
    getHospitilizations patientId Load


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load (Ok t) ->
            getLoadedState model t ! [ setLoadingStatus False ]

        Load (Err t) ->
            model ! [ setLoadingStatus False ]

        SetTableState newState ->
            { model | tableState = newState } ! []

        DropDownToggle recordId ->
            { model | hospitilizations = flipDropDownOpen model.hospitilizations recordId } ! []

        SendMenuMessage recordId messageType ->
            model ! [ sendMenuMessage (MenuMessage messageType recordId Nothing Nothing) ]

        DeleteHospitilizationConfirmed rowId ->
            let
                updatedRecords =
                    model.hospitilizations |> List.filter (\t -> t.id /= rowId)
            in
                { model | hospitilizations = updatedRecords, patientId = 7000 } ! [ deleteHospitilization rowId ]

        DeleteCompleted (Ok responseMsg) ->
            case getResponseError responseMsg of
                Just t ->
                    model ! [ displayErrorMessage t ]

                Nothing ->
                    model ! [ displaySuccessMessage "Record deleted successfully!" ]

        DeleteCompleted (Err t) ->
            model ! [ displayErrorMessage (toString t) ]

        Edit hospitilizationId ->
            model ! [ navHospitilizationsAddEdit hospitilizationId ]

        SetFilter filterState ->
            { model | filterFields = filterFields model.filterFields filterState } ! []

        AddNewStart hospitilizationId ->
            model ! [ navHospitilizationsAddEdit Nothing ]


view : Model -> Maybe AddEditDataSource -> Html Msg
view model addEditDataSource =
    div []
        [ case addEditDataSource of
            Just _ ->
                button [ type_ "button", class "btn btn-sm btn-default margin-bottom-5", onClick (AddNewStart Nothing) ] [ text "New Record" ]

            Nothing ->
                button [ type_ "button", class "btn btn-sm btn-default margin-bottom-5 disabled" ] [ text "New Record" ]
        , div [ class "e-grid e-js e-waitingpopup" ]
            [ Table.view (config SetFilter) model.tableState (filteredRecords model) ]
        ]


getColumns : List (Table.Column HospitilizationsRow Msg)
getColumns =
    [ Table.stringColumn "ID" (\t -> toString t.id)
    , Table.stringColumn "Facility Name" (\t -> defaultString t.facilityName)
    , Table.stringColumn "Date Of Admission" (\t -> defaultDate t.dateOfAdmission)
    , Table.stringColumn "Admit Problem" (\t -> defaultString t.admitProblem)
    , Table.stringColumn "Date Of Discharge" (\t -> defaultDate t.dateOfDischarge)
    , Table.stringColumn "Discharge Problem" (\t -> defaultString t.dischargeProblem)
    , Table.stringColumn "Svc Type" (\t -> defaultString t.serviceType)
    , checkColumn "Is From TCM" (\t -> t.fromTcm)
    , customColumn
    , rowDropDownColumn
    ]


customColumn : Table.Column HospitilizationsRow Msg
customColumn =
    Table.veryCustomColumn
        { name = "Has File"
        , viewData = viewCustomColumn
        , sorter = Table.unsortable
        }


viewCustomColumn : HospitilizationsRow -> Table.HtmlDetails Msg
viewCustomColumn { recordId } =
    Table.HtmlDetails []
        [ case recordId of
            Just t ->
                div [ class "RecordTableHref", onClick (SendMenuMessage t "ViewFile") ] [ text "File" ]

            Nothing ->
                div [] []
        ]


rowDropDownColumn : Table.Column HospitilizationsRow Msg
rowDropDownColumn =
    Table.veryCustomColumn
        { name = ""
        , viewData = \t -> rowDropDownDiv t.dropDownOpen (onClick (DropDownToggle t.id)) (dropDownItems t.id)
        , sorter = Table.unsortable
        }


dropDownItems : Int -> List ( String, String, Html.Attribute Msg )
dropDownItems rowId =
    [ ( "e-edit", "Edit", onClick (Edit (Just rowId)) )
    , ( "e-contextdelete", "Delete", onClick (SendMenuMessage rowId "HospitilizationDelete") )
    ]


config : (FilterState -> Msg) -> Table.Config HospitilizationsRow Msg
config event =
    Table.customConfig
        { toId = \t -> toString t.id
        , toMsg = SetTableState
        , columns = getColumns
        , customizations =
            { defaultCustomizations | tableAttrs = standardTableAttrs "RecordTable", thead = standardThead event }
        }


getExistingHospitilization : Maybe Int -> Model -> Maybe RecordAddNewInitData
getExistingHospitilization hospitilizationId model =
    case hospitilizationId of
        Just hospId ->
            let
                hospitilizations =
                    model.hospitilizations
                        |> List.filter (\t -> t.id == hospId)
                        |> List.head
            in
                case hospitilizations of
                    Just t ->
                        Just
                            { id = Just t.id
                            , patientId = t.patientId
                            , facilityId = t.facilityId
                            , patientReported = t.patientReported
                            , hospitalizationId = t.hospitalizationId
                            , dateOfAdmission = t.dateOfAdmission
                            , dateOfDischarge = t.dateOfDischarge
                            , hospitalServiceTypeId = t.hospitalServiceTypeId
                            , chiefComplaint = t.chiefComplaint
                            , admitDiagnosisId = t.admitDiagnosisId
                            , dischargeDiagnosisId = t.dischargeDiagnosisId
                            , dischargeRecommendations = t.dischargeRecommendations
                            , dischargePhysicianId = t.dischargePhysicianId
                            , facilityId2 = t.facilityId2
                            , dateOfAdmission2 = t.dateOfAdmission2
                            , dateOfDischarge2 = t.dateOfDischarge2
                            }

                    Nothing ->
                        Nothing

        Nothing ->
            Nothing
