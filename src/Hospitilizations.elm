port module Hospitilizations exposing (Msg, Model, emptyModel, subscriptions, init, update, view)

import Html exposing (Html, text, div, button, h4)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Common.Table as Table exposing (defaultCustomizations)
import Common.Grid exposing (checkColumn, standardTableAttrs, standardTheadNoFilters)
import Common.Types exposing (MenuMessage, RequiredType(Optional, Required), DropdownItem, AddEditDataSource)
import Common.Functions as Functions exposing (defaultString, defaultDate, sendMenuMessage, setUnsavedChanges, maybeVal, maybeToDateString)
import Common.Route as Route
import Common.Html
    exposing
        ( InputControlType(CheckInput, DropInputWithButton, AreaInput, TextInput, DateInput, KnockInput, DropInput)
        , getValidationErrors
        , defaultConfig
        , fullWidth
        , makeControls
        )
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


port initHospitilizations : SyncfusionData -> Cmd msg


port updateHospitilizations : (SyncfusionData -> msg) -> Sub msg


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Functions.deleteConfirmed DeleteHospitilizationConfirmed
        , updateHospitilizations UpdateHospitilizationsInitData
        ]


init : Int -> Cmd Msg
init patientId =
    load patientId


type alias Model =
    { rows : List HospitilizationsRow
    , facilityId : Maybe Int
    , tableState : Table.State
    , editData : Maybe EditData
    , showValidationErrors : Bool
    }


type alias EditData =
    { sfData : SyncfusionData
    , patientReported : Bool
    , chiefComplaint : String
    , dischargeRecommendations : String
    }


type alias SyncfusionData =
    { id : Maybe Int
    , facilities : List DropdownItem
    , hospitilizationServiceTypes : List DropdownItem
    , hospitalizationDischargePhysicians : List DropdownItem
    , facilityId : Maybe Int
    , admitDiagnosisId : Maybe Int
    , dischargeDiagnosisId : Maybe Int
    , facilityId2 : Maybe Int
    , hospitalServiceTypeId : Maybe Int
    , dischargePhysicianId : Maybe Int
    , dateOfAdmission : Maybe String
    , dateOfDischarge : Maybe String
    , dateOfAdmission2 : Maybe String
    , dateOfDischarge2 : Maybe String
    }


type alias HospitilizationsRow =
    { id : Int
    , facilityName : Maybe String
    , dateOfAdmission : Maybe String
    , admitProblem : Maybe String
    , dateOfDischarge : Maybe String
    , dischargeProblem : Maybe String
    , serviceType : Maybe String
    , fromTcm : Bool
    , recordId : Maybe Int
    , dropdownOpen : Bool

    -- for edit
    , patientId : Int
    , facilityId : Maybe Int
    , patientReported : Bool
    , hospitalServiceTypeId : Maybe Int
    , chiefComplaint : String
    , admitDiagnosisId : Maybe Int
    , dischargeDiagnosisId : Maybe Int
    , dischargeRecommendations : String
    , dischargePhysicianId : Maybe Int
    , facilityId2 : Maybe Int
    , dateOfAdmission2 : Maybe String
    , dateOfDischarge2 : Maybe String
    }


formInputs : EditData -> List (InputControlType Msg)
formInputs editData =
    [ CheckInput "Patient Reported" Optional editData.patientReported (UpdatePatientReported editData)
    , DropInputWithButton "Facility Name" Required editData.sfData.facilityId "FacilityId" "Add New Facility"
    , DateInput "Date of Admission" Required (defaultString editData.sfData.dateOfAdmission) "DateOfAdmissionId"
    , DateInput "Date of Discharge" Required (defaultString editData.sfData.dateOfDischarge) "DateOfDischargeId"
    , DropInput "Hospital Service Type" Required editData.sfData.hospitalServiceTypeId "HospitalServiceTypeId"
    , AreaInput "Chief Complaint" Required editData.chiefComplaint (UpdateChiefComplaint editData)
    , KnockInput "Admit Diagnosis" Required "HospitalizationAdmitProblemSelection"
    , KnockInput "Discharge Diagnosis" Required "HospitalizationDischargeProblemSelection"
    , TextInput "Discharge Recommendations" Required editData.dischargeRecommendations (UpdateDischargeRecommendations editData)
    , DropInputWithButton "Discharge Physician" Optional editData.sfData.dischargePhysicianId "DischargePhysicianId" "New Provider"
    , DropInputWithButton "Secondary Facility Name" Optional editData.sfData.facilityId2 "FacilityId2" "Add New Facility"
    , DateInput "Secondary Date of Admission" Optional (defaultString editData.sfData.dateOfAdmission) "DateOfAdmissionId2"
    , DateInput "Secondary Date of Discharge" Optional (defaultString editData.sfData.dateOfDischarge) "DateOfDischargeId2"
    ]


view : Model -> Maybe AddEditDataSource -> Html Msg
view model addEditDataSource =
    case model.editData of
        Nothing ->
            div []
                [ h4 [] [ text "Hospitilizations" ]
                , div [ class "e-grid e-js e-waitingpopup" ]
                    [ Table.view (config addEditDataSource model.tableState) model.tableState model.rows ]
                ]

        Just editData ->
            let
                errors =
                    getValidationErrors (formInputs editData)

                validationErrorsDiv =
                    if model.showValidationErrors == True && List.length errors > 0 then
                        div [ class "error margin-bottom-10" ] (List.map (\t -> div [] [ text t ]) errors)
                    else
                        div [] []
            in
                div [ class "form-horizontal" ]
                    [ validationErrorsDiv
                    , makeControls defaultConfig (formInputs editData)
                    , div [ class "form-group" ]
                        [ div [ class fullWidth ]
                            [ button [ type_ "button", onClick (Save editData), class "btn btn-sm btn-success" ] [ text "Save" ]
                            , button [ type_ "button", onClick Cancel, class "btn btn-sm btn-default margin-left-5" ] [ text "Cancel" ]
                            ]
                        ]
                    ]


type Msg
    = Load (Result Http.Error (List HospitilizationsRow))
    | SetTableState Table.State
    | DeletePrompt Int
    | DeleteHospitilizationConfirmed Int
    | DeleteCompleted (Result Http.Error String)
    | Add AddEditDataSource
    | Edit HospitilizationsRow AddEditDataSource
    | SendMenuMessage Int String
    | Save EditData
    | SaveCompleted (Result Http.Error String)
    | Cancel
    | UpdateHospitilizationsInitData SyncfusionData
    | UpdatePatientReported EditData Bool
    | UpdateChiefComplaint EditData String
    | UpdateDischargeRecommendations EditData String


update : Msg -> Model -> Int -> ( Model, Cmd Msg )
update msg model patientId =
    let
        updateAddNew t =
            t ! [ setUnsavedChanges True ]
    in
        case msg of
            Load (Ok t) ->
                getLoadedState model t ! [ Functions.setLoadingStatus False ]

            Load (Err t) ->
                model ! [ Functions.displayErrorMessage (toString t) ]

            SetTableState newState ->
                { model | tableState = newState } ! []

            SendMenuMessage recordId messageType ->
                model ! [ sendMenuMessage (MenuMessage messageType recordId Nothing Nothing) ]

            DeletePrompt rowId ->
                model ! [ Functions.deletePrompt rowId ]

            DeleteHospitilizationConfirmed rowId ->
                let
                    newHospitilizations =
                        model.rows |> List.filter (\t -> t.id /= rowId)
                in
                    { model | rows = newHospitilizations }
                        ! [ deleteHospitilization rowId DeleteCompleted ]

            DeleteCompleted (Ok responseMsg) ->
                case Functions.getResponseError responseMsg of
                    Just t ->
                        model ! [ Functions.displayErrorMessage t, load patientId ]

                    Nothing ->
                        model ! [ Functions.displaySuccessMessage "Record deleted successfully!" ]

            DeleteCompleted (Err t) ->
                model ! [ Functions.displayErrorMessage (toString t) ]

            Add addEditDataSource ->
                let
                    editData =
                        getEditData addEditDataSource Nothing
                in
                    { model | editData = Just editData } ! [ initHospitilizations editData.sfData ]

            Edit row addEditDataSource ->
                let
                    editData =
                        getEditData addEditDataSource (Just row)
                in
                    { model | editData = Just editData } ! [ initHospitilizations editData.sfData ]

            -- edit
            Save editData ->
                if List.length (getValidationErrors (formInputs editData)) > 0 then
                    { model | showValidationErrors = True } ! []
                else
                    let
                        body =
                            encodeEditData editData patientId
                    in
                        model
                            ! [ Functions.postRequest body "/People/AddEditHospitilization"
                                    |> Http.send SaveCompleted
                              , setUnsavedChanges False
                              ]

            SaveCompleted (Ok responseMsg) ->
                case Functions.getResponseError responseMsg of
                    Just t ->
                        model ! [ Functions.displayErrorMessage t ]

                    Nothing ->
                        model
                            ! [ Functions.displaySuccessMessage "Save completed successfully!"
                              , Route.modifyUrl Route.Hospitilizations
                              ]

            SaveCompleted (Err t) ->
                model ! [ Functions.displayErrorMessage (toString t) ]

            Cancel ->
                model ! [ setUnsavedChanges False, Route.modifyUrl Route.Hospitilizations ]

            UpdateHospitilizationsInitData sfData ->
                let
                    newEditData =
                        case model.editData of
                            Just editData ->
                                Just { editData | sfData = sfData }

                            Nothing ->
                                Nothing
                in
                    { model | editData = newEditData } ! []

            UpdatePatientReported editData t ->
                updateAddNew { model | editData = Just { editData | patientReported = t } }

            UpdateChiefComplaint editData t ->
                updateAddNew { model | editData = Just { editData | chiefComplaint = t } }

            UpdateDischargeRecommendations editData t ->
                updateAddNew { model | editData = Just { editData | dischargeRecommendations = t } }


getColumns : Maybe AddEditDataSource -> Table.State -> List (Table.Column HospitilizationsRow Msg)
getColumns addEditDataSource state =
    let
        dropDownItems row =
            case addEditDataSource of
                Just t ->
                    [ ( "e-edit", "Edit", onClick (Edit row t) )
                    , ( "e-contextdelete", "Delete", onClick (DeletePrompt row.id) )
                    ]

                Nothing ->
                    []
    in
        [ Table.stringColumn "ID" (\t -> toString t.id)
        , Table.stringColumn "Facility Name" (\t -> defaultString t.facilityName)
        , Table.stringColumn "Date Of Admission" (\t -> defaultDate t.dateOfAdmission)
        , Table.stringColumn "Admit Problem" (\t -> defaultString t.admitProblem)
        , Table.stringColumn "Date Of Discharge" (\t -> defaultDate t.dateOfDischarge)
        , Table.stringColumn "Discharge Problem" (\t -> defaultString t.dischargeProblem)
        , Table.stringColumn "Svc Type" (\t -> defaultString t.serviceType)
        , checkColumn "Is From TCM" (\t -> t.fromTcm)
        , customColumn
        , Table.dropdownColumn (\t -> Table.dropdownDetails (dropDownItems t) t.id state SetTableState)
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


config : Maybe AddEditDataSource -> Table.State -> Table.Config HospitilizationsRow Msg
config addEditDataSource state =
    let
        buttons =
            case addEditDataSource of
                Just t ->
                    [ ( "e-addnew", onClick (Add t) ) ]

                Nothing ->
                    []
    in
        Table.customConfig
            { toId = \t -> toString t.id
            , toMsg = SetTableState
            , columns = getColumns addEditDataSource state
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
        |> Pipeline.required "Id" Decode.int
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


deleteHospitilization : a -> (Result Http.Error String -> msg) -> Cmd msg
deleteHospitilization rowId deleteCompleted =
    Http.send deleteCompleted <| Http.getString ("/People/DeleteHospitilization?id=" ++ toString rowId)


getLoadedState : Model -> List HospitilizationsRow -> Model
getLoadedState model hospitilizationsRow =
    { model | rows = hospitilizationsRow }


emptyModel : Model
emptyModel =
    { editData = Nothing
    , rows = []
    , facilityId = Nothing
    , tableState = Table.initialSort "Date"
    , showValidationErrors = False
    }


emptyHospitilizationRow : HospitilizationsRow
emptyHospitilizationRow =
    { id = -1
    , facilityName = Nothing
    , dateOfAdmission = Nothing
    , admitProblem = Nothing
    , dateOfDischarge = Nothing
    , dischargeProblem = Nothing
    , serviceType = Nothing
    , fromTcm = False
    , recordId = Nothing
    , dropdownOpen = False

    -- for edit
    , patientId = 0
    , facilityId = Nothing
    , patientReported = False
    , hospitalServiceTypeId = Nothing
    , chiefComplaint = ""
    , admitDiagnosisId = Nothing
    , dischargeDiagnosisId = Nothing
    , dischargeRecommendations = ""
    , dischargePhysicianId = Nothing
    , facilityId2 = Nothing
    , dateOfAdmission2 = Nothing
    , dateOfDischarge2 = Nothing
    }


getEditData : AddEditDataSource -> Maybe HospitilizationsRow -> EditData
getEditData addEditDataSource maybeRow =
    case maybeRow of
        Just row ->
            { sfData = getHospitilizationsInitData addEditDataSource maybeRow
            , patientReported = row.patientReported
            , chiefComplaint = row.chiefComplaint
            , dischargeRecommendations = row.dischargeRecommendations
            }

        Nothing ->
            { sfData = getHospitilizationsInitData addEditDataSource maybeRow
            , patientReported = False
            , chiefComplaint = ""
            , dischargeRecommendations = ""
            }


getHospitilizationsInitData : AddEditDataSource -> Maybe HospitilizationsRow -> SyncfusionData
getHospitilizationsInitData addEditDataSource maybeHospitilizationsRow =
    let
        hospitilizationsRow =
            Maybe.withDefault emptyHospitilizationRow maybeHospitilizationsRow
    in
        { id =
            if hospitilizationsRow.id == -1 then
                Nothing
            else
                Just hospitilizationsRow.id
        , facilities = addEditDataSource.facilities
        , hospitilizationServiceTypes = addEditDataSource.hospitilizationServiceTypes
        , hospitalizationDischargePhysicians = addEditDataSource.hospitalizationDischargePhysicians
        , facilityId = addEditDataSource.facilityId
        , admitDiagnosisId = hospitilizationsRow.admitDiagnosisId
        , dischargeDiagnosisId = hospitilizationsRow.dischargeDiagnosisId
        , facilityId2 = hospitilizationsRow.facilityId2
        , hospitalServiceTypeId = hospitilizationsRow.hospitalServiceTypeId
        , dischargePhysicianId = hospitilizationsRow.dischargePhysicianId
        , dateOfAdmission = hospitilizationsRow.dateOfAdmission
        , dateOfDischarge = hospitilizationsRow.dateOfDischarge
        , dateOfAdmission2 = hospitilizationsRow.dateOfAdmission2
        , dateOfDischarge2 = hospitilizationsRow.dateOfDischarge2
        }


encodeEditData : EditData -> Int -> Encode.Value
encodeEditData newRecord patientId =
    Encode.object
        [ ( "Id", maybeVal Encode.int <| newRecord.sfData.id )
        , ( "PatientId", Encode.int <| patientId )
        , ( "FacilityId", maybeVal Encode.int <| newRecord.sfData.facilityId )
        , ( "PatientReported", Encode.bool <| newRecord.patientReported )
        , ( "DateOfAdmission", maybeVal Encode.string <| maybeToDateString <| newRecord.sfData.dateOfAdmission )
        , ( "DateOfDischarge", maybeVal Encode.string <| maybeToDateString <| newRecord.sfData.dateOfDischarge )
        , ( "HospitalServiceTypeId", maybeVal Encode.int <| newRecord.sfData.hospitalServiceTypeId )
        , ( "ChiefComplaint", Encode.string <| newRecord.chiefComplaint )
        , ( "AdmitDiagnosisId", maybeVal Encode.int <| newRecord.sfData.admitDiagnosisId )
        , ( "DischargeDiagnosisId", maybeVal Encode.int <| newRecord.sfData.dischargeDiagnosisId )
        , ( "DischargeRecommendations", Encode.string <| newRecord.dischargeRecommendations )
        , ( "DischargePhysicianId", maybeVal Encode.int <| newRecord.sfData.dischargePhysicianId )
        , ( "FacilityId2", maybeVal Encode.int <| newRecord.sfData.facilityId2 )
        , ( "DateOfAdmission2", maybeVal Encode.string <| maybeToDateString <| newRecord.sfData.dateOfAdmission2 )
        , ( "DateOfDischarge2", maybeVal Encode.string <| maybeToDateString <| newRecord.sfData.dateOfDischarge2 )
        ]


load : Int -> Cmd Msg
load patientId =
    Decode.list decodeHospitilizationsRow
        |> Http.get ("/People/HospitilizationsGrid?patientId=" ++ toString patientId)
        |> Http.send Load
