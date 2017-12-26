port module Hospitilizations exposing (Msg, Model, emptyModel, subscriptions, init, update, view)

import Html exposing (Html, text, div, button)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Common.Table as Table exposing (defaultCustomizations)
import Common.Grid exposing (checkColumn, standardTableAttrs, standardThead, rowDropDownDiv, standardTheadNoFilters)
import Common.Types exposing (MenuMessage, FilterState, AddEditDataSource, HospitilizationsRow)
import Common.Functions as Functions exposing (defaultString, defaultDate, defaultLower)
import Common.Ports exposing (sendMenuMessage)
import Common.Route as Route
import Common.Mouse as Mouse
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline


port deleteHospitilizationConfirmed : (Int -> msg) -> Sub msg


subscriptions : List HospitilizationsRow -> Sub Msg
subscriptions rows =
    Sub.batch
        [ deleteHospitilizationConfirmed DeleteHospitilizationConfirmed
        , if Functions.anyDropdownOpon rows then
            Mouse.clicks Blur
          else
            Sub.none
        ]


init : Int -> Cmd Msg
init patientId =
    getHospitilizations patientId Load


type alias Model =
    { rows : List HospitilizationsRow
    , facilityId : Maybe Int
    , tableState : Table.State
    }


view : Model -> Maybe AddEditDataSource -> Html Msg
view model addEditDataSource =
    div []
        [ div [ class "e-grid e-js e-waitingpopup" ]
            [ Table.view (config addEditDataSource) model.tableState model.rows ]
        ]


type Msg
    = Load (Result Http.Error (List HospitilizationsRow))
    | Blur Mouse.Position
    | SetTableState Table.State
    | DropDownToggle (Maybe Int)
    | DeleteHospitilizationConfirmed Int
    | DeleteCompleted (Result Http.Error String)
    | Add
    | Edit Int
    | SendMenuMessage Int String


update : Msg -> Model -> Int -> ( Model, Cmd Msg )
update msg model _ =
    case msg of
        Load (Ok t) ->
            getLoadedState model t ! [ Functions.setLoadingStatus False ]

        Load (Err t) ->
            model ! [ Functions.displayErrorMessage (toString t) ]

        SetTableState newState ->
            { model | tableState = newState } ! []

        DropDownToggle recordId ->
            { model | rows = Functions.flipDropdownOpen model.rows recordId } ! []

        SendMenuMessage recordId messageType ->
            model ! [ sendMenuMessage (MenuMessage messageType recordId Nothing Nothing) ]

        DeleteHospitilizationConfirmed rowId ->
            let
                newHospitilizations =
                    model.rows |> List.filter (\t -> t.id /= Just rowId)
            in
                { model | rows = newHospitilizations }
                    ! [ deleteHospitilization rowId DeleteCompleted ]

        DeleteCompleted (Ok responseMsg) ->
            case Functions.getResponseError responseMsg of
                Just t ->
                    model ! [ Functions.displayErrorMessage t, Route.refresh ]

                Nothing ->
                    model ! [ Functions.displaySuccessMessage "Record deleted successfully!" ]

        DeleteCompleted (Err t) ->
            model ! [ Functions.displayErrorMessage (toString t) ]

        Add ->
            model ! [ Route.modifyUrl Route.HospitilizationsAdd ]

        Edit rowId ->
            model ! [ Route.modifyUrl (Route.HospitilizationsEdit rowId) ]

        Blur position ->
            { model
                | rows = Functions.closeDropdowns model.rows position.target
            }
                ! []


getColumns : List (Table.Column HospitilizationsRow Msg)
getColumns =
    [ Table.stringColumn "ID" (\t -> Functions.defaultIntToString t.id)
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
        , viewData = \t -> rowDropDownDiv t.dropdownOpen (onClick (DropDownToggle t.id)) (dropDownItems <| Functions.defaultInt t.id)
        , sorter = Table.unsortable
        }


dropDownItems : Int -> List ( String, String, Html.Attribute Msg )
dropDownItems rowId =
    [ ( "e-edit", "Edit", onClick (Edit rowId) )
    , ( "e-contextdelete", "Delete", onClick (SendMenuMessage rowId "HospitilizationDelete") )
    ]


config : Maybe AddEditDataSource -> Table.Config HospitilizationsRow Msg
config addEditDataSource =
    let
        buttons =
            case addEditDataSource of
                Just t ->
                    [ ( "e-addnew", onClick Add ) ]

                Nothing ->
                    []
    in
        Table.customConfig
            { toId = \t -> toString t.id
            , toMsg = SetTableState
            , columns = getColumns
            , customizations =
                { defaultCustomizations
                    | tableAttrs = standardTableAttrs "RecordTable"
                    , thead = standardTheadNoFilters
                    , theadButtons = buttons
                }
            }


decodeHospitilizationsRow : Decode.Decoder HospitilizationsRow
decodeHospitilizationsRow =
    Pipeline.decode HospitilizationsRow
        |> Pipeline.required "Id" (Decode.maybe Decode.int)
        |> Pipeline.required "FacilityName" (Decode.maybe Decode.string)
        |> Pipeline.required "DateOfAdmission" (Decode.maybe Decode.string)
        |> Pipeline.required "AdmitProblem" (Decode.maybe Decode.string)
        |> Pipeline.required "DateOfDischarge" (Decode.maybe Decode.string)
        |> Pipeline.required "DischargeProblem" (Decode.maybe Decode.string)
        |> Pipeline.required "ServiceType" (Decode.maybe Decode.string)
        |> Pipeline.required "FromTcm" Decode.bool
        |> Pipeline.required "RecordId" (Decode.maybe Decode.int)
        |> Pipeline.hardcoded False
        -- For edit only
        |> Pipeline.required "PatientId" Decode.int
        |> Pipeline.required "FacilityId" (Decode.maybe Decode.int)
        |> Pipeline.required "PatientReported" Decode.bool
        |> Pipeline.required "HospitalServiceTypeId" (Decode.maybe Decode.int)
        |> Pipeline.required "ChiefComplaint" Decode.string
        |> Pipeline.required "AdmitDiagnosisId" (Decode.maybe Decode.int)
        |> Pipeline.required "DischargeDiagnosisId" (Decode.maybe Decode.int)
        |> Pipeline.required "DischargeRecommendations" Decode.string
        |> Pipeline.required "DischargePhysicianId" (Decode.maybe Decode.int)
        |> Pipeline.required "FacilityId2" (Decode.maybe Decode.int)
        |> Pipeline.required "DateOfAdmission2" (Decode.maybe Decode.string)
        |> Pipeline.required "DateOfDischarge2" (Decode.maybe Decode.string)


request : Int -> Http.Request (List HospitilizationsRow)
request patientId =
    Decode.list decodeHospitilizationsRow
        |> Http.get ("/People/HospitilizationsGrid?patientId=" ++ toString patientId)


getHospitilizations : Int -> (Result Http.Error (List HospitilizationsRow) -> msg) -> Cmd msg
getHospitilizations patientId t =
    Http.send t (request patientId)


deleteHospitilization : a -> (Result Http.Error String -> msg) -> Cmd msg
deleteHospitilization rowId deleteCompleted =
    Http.send deleteCompleted <| Http.getString ("/People/DeleteHospitilization?id=" ++ toString rowId)


getLoadedState : Model -> List HospitilizationsRow -> Model
getLoadedState model hospitilizationsRow =
    { model | rows = hospitilizationsRow }


emptyModel : Model
emptyModel =
    { rows = []
    , facilityId = Nothing
    , tableState = Table.initialSort "Date"
    }
