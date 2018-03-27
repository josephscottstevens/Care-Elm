port module Hospitilizations exposing (Msg, Model, emptyModel, subscriptions, init, update, view)

import Html exposing (Html, text, div, button, h4)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Common.Types exposing (MenuMessage, RequiredType(Optional, Required), DropdownItem, AddEditDataSource)
import Common.Functions as Functions exposing (defaultString, sendMenuMessage, setUnsavedChanges, maybeVal)
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
import Common.Table as Table


port initHospitilizations : SyncfusionData -> Cmd msg


port updateHospitilizations : (SyncfusionData -> msg) -> Sub msg


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Functions.dialogConfirmed DeleteHospitilizationConfirmed
        , updateHospitilizations UpdateHospitilizationsInitData
        ]


init : Int -> Cmd Msg
init patientId =
    load patientId


type alias Model =
    { rows : List Row
    , facilityId : Maybe Int
    , tableState : Table.State
    , editData : Maybe EditData
    , showValidationErrors : Bool
    }


type alias EditData =
    { sfData : SyncfusionData
    , patientReported : Bool
    , chiefComplaint : Maybe String
    , dischargeRecommendations : Maybe String
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


type alias Row =
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
    , chiefComplaint : Maybe String
    , admitDiagnosisId : Maybe Int
    , dischargeDiagnosisId : Maybe Int
    , dischargeRecommendations : Maybe String
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
                , Table.view model.tableState model.rows (gridConfig addEditDataSource) Nothing
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
    = Load (Result Http.Error (List Row))
    | SetTableState Table.State
    | DeletePrompt Row
    | DeleteHospitilizationConfirmed Int
    | DeleteCompleted (Result Http.Error String)
    | Add AddEditDataSource
    | Edit AddEditDataSource Row
    | SendMenuMessage Int String
    | Save EditData
    | SaveCompleted (Result Http.Error String)
    | NoOp
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

            DeletePrompt row ->
                model ! [ Functions.deleteDialogShow row.id ]

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

            Edit addEditDataSource row ->
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
                    model
                        ! [ "/People/AddEditHospitilization"
                                |> Functions.postRequest (encodeEditData editData patientId)
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

            NoOp ->
                model ! []

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
                updateAddNew { model | editData = Just { editData | chiefComplaint = Just t } }

            UpdateDischargeRecommendations editData t ->
                updateAddNew { model | editData = Just { editData | dischargeRecommendations = Just t } }


getColumns : Maybe AddEditDataSource -> List (Table.Column Row Msg)
getColumns addEditDataSource =
    let
        dropDownItems =
            case addEditDataSource of
                Just t ->
                    [ ( "e-edit", "Edit", Edit t )
                    , ( "e-contextdelete", "Delete", DeletePrompt )
                    ]

                Nothing ->
                    []
    in
        [ Table.intColumn "ID" (\t -> Just <| t.id)
        , Table.stringColumn "Facility Name" (\t -> t.facilityName)
        , Table.dateColumn "Date Of Admission" (\t -> t.dateOfAdmission)
        , Table.stringColumn "Admit Problem" (\t -> t.admitProblem)
        , Table.dateColumn "Date Of Discharge" (\t -> t.dateOfDischarge)
        , Table.stringColumn "Discharge Problem" (\t -> t.dischargeProblem)
        , Table.stringColumn "Svc Type" (\t -> t.serviceType)
        , Table.checkColumn "Is From TCM" (\t -> t.fromTcm)
        , Table.hrefColumnExtra "File" hrefCustom
        , Table.dropdownColumn dropDownItems
        ]


hrefCustom : { a | recordId : Maybe Int } -> Html Msg
hrefCustom row =
    case row.recordId of
        Just t ->
            div [ class "RecordTableHref", onClick (SendMenuMessage t "ViewFile") ] [ text "File" ]

        Nothing ->
            div [] []


gridConfig : Maybe AddEditDataSource -> Table.Config Row Msg
gridConfig addEditDataSource =
    { domTableId = "HospitilizationsTable"
    , toolbar =
        case addEditDataSource of
            Just t ->
                [ ( "e-addnew e-loaded", Add t ) ]

            Nothing ->
                [ ( "e-addnew e-disable", NoOp ) ]
    , toMsg = SetTableState
    , columns = getColumns addEditDataSource
    }


decodeRow : Decode.Decoder Row
decodeRow =
    Pipeline.decode Row
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
        |> Pipeline.required "ChiefComplaint" (Decode.maybe Decode.string)
        |> Pipeline.required "AdmitDiagnosisId" (Decode.maybe Decode.int)
        |> Pipeline.required "DischargeDiagnosisId" (Decode.maybe Decode.int)
        |> Pipeline.required "DischargeRecommendations" (Decode.maybe Decode.string)
        |> Pipeline.required "DischargePhysicianId" (Decode.maybe Decode.int)
        |> Pipeline.required "FacilityId2" (Decode.maybe Decode.int)
        |> Pipeline.required "DateOfAdmission2" (Decode.maybe Decode.string)
        |> Pipeline.required "DateOfDischarge2" (Decode.maybe Decode.string)


deleteHospitilization : a -> (Result Http.Error String -> msg) -> Cmd msg
deleteHospitilization rowId deleteCompleted =
    Http.send deleteCompleted <| Http.getString ("/People/DeleteHospitilization?id=" ++ toString rowId)


getLoadedState : Model -> List Row -> Model
getLoadedState model rows =
    { model | rows = rows }


emptyModel : Model
emptyModel =
    { editData = Nothing
    , rows = []
    , facilityId = Nothing
    , tableState = Table.init "Date"
    , showValidationErrors = False
    }


emptyRow : Row
emptyRow =
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
    , chiefComplaint = Nothing
    , admitDiagnosisId = Nothing
    , dischargeDiagnosisId = Nothing
    , dischargeRecommendations = Nothing
    , dischargePhysicianId = Nothing
    , facilityId2 = Nothing
    , dateOfAdmission2 = Nothing
    , dateOfDischarge2 = Nothing
    }


getEditData : AddEditDataSource -> Maybe Row -> EditData
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
            , chiefComplaint = Nothing
            , dischargeRecommendations = Nothing
            }


getHospitilizationsInitData : AddEditDataSource -> Maybe Row -> SyncfusionData
getHospitilizationsInitData addEditDataSource maybeRow =
    let
        row =
            Maybe.withDefault emptyRow maybeRow
    in
        { id =
            if row.id == -1 then
                Nothing
            else
                Just row.id
        , facilities = addEditDataSource.facilities
        , hospitilizationServiceTypes = addEditDataSource.hospitilizationServiceTypes
        , hospitalizationDischargePhysicians = addEditDataSource.hospitalizationDischargePhysicians
        , facilityId = addEditDataSource.facilityId
        , admitDiagnosisId = row.admitDiagnosisId
        , dischargeDiagnosisId = row.dischargeDiagnosisId
        , facilityId2 = row.facilityId2
        , hospitalServiceTypeId = row.hospitalServiceTypeId
        , dischargePhysicianId = row.dischargePhysicianId
        , dateOfAdmission = row.dateOfAdmission
        , dateOfDischarge = row.dateOfDischarge
        , dateOfAdmission2 = row.dateOfAdmission2
        , dateOfDischarge2 = row.dateOfDischarge2
        }


encodeEditData : EditData -> Int -> Encode.Value
encodeEditData newRecord patientId =
    Encode.object
        [ ( "Id", maybeVal Encode.int <| newRecord.sfData.id )
        , ( "PatientId", Encode.int <| patientId )
        , ( "FacilityId", maybeVal Encode.int <| newRecord.sfData.facilityId )
        , ( "PatientReported", Encode.bool <| newRecord.patientReported )
        , ( "DateOfAdmission", maybeVal Encode.string <| newRecord.sfData.dateOfAdmission )
        , ( "DateOfDischarge", maybeVal Encode.string <| newRecord.sfData.dateOfDischarge )
        , ( "HospitalServiceTypeId", maybeVal Encode.int <| newRecord.sfData.hospitalServiceTypeId )
        , ( "ChiefComplaint", maybeVal Encode.string <| newRecord.chiefComplaint )
        , ( "AdmitDiagnosisId", maybeVal Encode.int <| newRecord.sfData.admitDiagnosisId )
        , ( "DischargeDiagnosisId", maybeVal Encode.int <| newRecord.sfData.dischargeDiagnosisId )
        , ( "DischargeRecommendations", maybeVal Encode.string <| newRecord.dischargeRecommendations )
        , ( "DischargePhysicianId", maybeVal Encode.int <| newRecord.sfData.dischargePhysicianId )
        , ( "FacilityId2", maybeVal Encode.int <| newRecord.sfData.facilityId2 )
        , ( "DateOfAdmission2", maybeVal Encode.string <| newRecord.sfData.dateOfAdmission2 )
        , ( "DateOfDischarge2", maybeVal Encode.string <| newRecord.sfData.dateOfDischarge2 )
        ]


load : Int -> Cmd Msg
load patientId =
    Decode.list decodeRow
        |> Http.get ("/People/HospitilizationsGrid?patientId=" ++ toString patientId)
        |> Http.send Load
