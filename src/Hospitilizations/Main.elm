module Hospitilizations.Main exposing (..)

import Hospitilizations.Functions exposing (..)
import Hospitilizations.Types exposing (..)
import Html exposing (Html, text, div, button, a)
import Html.Attributes exposing (class, type_, href)
import Html.Events exposing (onClick)
import Table exposing (..)
import Common.Grid exposing (..)
import Common.Types exposing (..)
import Common.Functions exposing (..)
import Ports exposing (..)
import Common.Routes exposing (navHospitilizationsAddEdit)


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ deleteConfirmed DeleteConfirmed
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

        DeleteConfirmed rowId ->
            let
                updatedRecords =
                    model.hospitilizations |> List.filter (\t -> t.id /= rowId)
            in
                { model | hospitilizations = updatedRecords } ! [ deleteRequest rowId ]

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
            model ! [ navHospitilizationsAddEdit ]


view : Model -> Maybe AddEditDataSource -> Html Msg
view model addEditDataSource =
    div []
        [ case addEditDataSource of
            Just _ ->
                button [ type_ "button", class "btn btn-sm btn-default margin-bottom-5", onClick AddNewStart ] [ text "New Record" ]

            Nothing ->
                button [ type_ "button", class "btn btn-sm btn-default margin-bottom-5 disabled" ] [ text "New Record" ]
        , div [ class "e-grid e-js e-waitingpopup" ]
            [ Table.view (config SetFilter) model.tableState (filteredRecords model) ]
        ]


getColumns : List (Column HospitilizationsRow Msg)
getColumns =
    [ stringColumn "ID" (\t -> toString t.id)
    , stringColumn "Facility Name" (\t -> defaultString t.facilityName)
    , stringColumn "Date Of Admission" (\t -> defaultDate t.dateOfAdmission)
    , stringColumn "Admit Problem" (\t -> defaultString t.admitProblem)
    , stringColumn "Date Of Discharge" (\t -> defaultDate t.dateOfDischarge)
    , stringColumn "Discharge Problem" (\t -> defaultString t.dischargeProblem)
    , stringColumn "Svc Type" (\t -> defaultString t.serviceType)
    , checkColumn "Is From TCM" (\t -> t.fromTcm)
    , customColumn
    , rowDropDownColumn
    ]


customColumn : Table.Column HospitilizationsRow Msg
customColumn =
    Table.veryCustomColumn
        { name = "hasFile"
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
    [ ( "e-contextdelete", "Delete", onClick (SendMenuMessage rowId "Delete") ) ]


config : (FilterState -> Msg) -> Config HospitilizationsRow Msg
config event =
    customConfig
        { toId = \t -> toString t.id
        , toMsg = SetTableState
        , columns = getColumns
        , customizations =
            { defaultCustomizations | tableAttrs = standardTableAttrs "RecordTable", thead = standardThead event }
        }
