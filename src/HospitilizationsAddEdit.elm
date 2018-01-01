port module HospitilizationsAddEdit exposing (Msg, Model, subscriptions, init, update, view, emptyModel)

import Html exposing (Html, text, div, button)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Common.Html
    exposing
        ( InputControlType(CheckInput, DropInputWithButton, AreaInput, TextInput, DateInput, KnockInput, DropInput)
        , getValidationErrors
        , defaultConfig
        , fullWidth
        , makeControls
        )
import Common.Types exposing (RequiredType(Optional, Required), DropdownItem, AddEditDataSource, HospitilizationsRow)
import Common.Functions as Functions exposing (setUnsavedChanges, defaultString, maybeVal, maybeToDateString)
import Common.Route as Route
import Http
import Json.Encode as Encode


port initHospitilizations : SyncfusionData -> Cmd msg


port updateHospitilizations : (SyncfusionData -> msg) -> Sub msg


subscriptions : Sub Msg
subscriptions =
    updateHospitilizations UpdateHospitilizationsInitData


init : AddEditDataSource -> Maybe HospitilizationsRow -> Cmd Msg
init addEditDataSource hospitilizationsRow =
    initHospitilizations (getHospitilizationsInitData addEditDataSource hospitilizationsRow)


type alias Model =
    { sfData : SyncfusionData
    , patientReported : Bool
    , chiefComplaint : String
    , dischargeRecommendations : String
    , showValidationErrors : Bool
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


view : Model -> Html Msg
view model =
    let
        errors =
            getValidationErrors (formInputs model)

        validationErrorsDiv =
            if model.showValidationErrors == True && List.length errors > 0 then
                div [ class "error margin-bottom-10" ] (List.map (\t -> div [] [ text t ]) errors)
            else
                div [] []
    in
        div [ class "form-horizontal" ]
            [ validationErrorsDiv
            , makeControls defaultConfig (formInputs model)
            , div [ class "form-group" ]
                [ div [ class fullWidth ]
                    [ button [ type_ "button", onClick Save, class "btn btn-sm btn-success" ] [ text "Save" ]
                    , button [ type_ "button", onClick Cancel, class "btn btn-sm btn-default margin-left-5" ] [ text "Cancel" ]
                    ]
                ]
            ]


formInputs : Model -> List (InputControlType Msg)
formInputs model =
    [ CheckInput "Patient Reported" Optional model.patientReported UpdatePatientReported
    , DropInputWithButton "Facility Name" Required model.sfData.facilityId "FacilityId" "Add New Facility"
    , DateInput "Date of Admission" Required (defaultString model.sfData.dateOfAdmission) "DateOfAdmissionId"
    , DateInput "Date of Discharge" Required (defaultString model.sfData.dateOfDischarge) "DateOfDischargeId"
    , DropInput "Hospital Service Type" Required model.sfData.hospitalServiceTypeId "HospitalServiceTypeId"
    , AreaInput "Chief Complaint" Required model.chiefComplaint UpdateChiefComplaint
    , KnockInput "Admit Diagnosis" Required "HospitalizationAdmitProblemSelection"
    , KnockInput "Discharge Diagnosis" Required "HospitalizationDischargeProblemSelection"
    , TextInput "Discharge Recommendations" Required model.dischargeRecommendations UpdateDischargeRecommendations
    , DropInputWithButton "Discharge Physician" Optional model.sfData.dischargePhysicianId "DischargePhysicianId" "New Provider"
    , DropInputWithButton "Secondary Facility Name" Optional model.sfData.facilityId2 "FacilityId2" "Add New Facility"
    , DateInput "Secondary Date of Admission" Optional (defaultString model.sfData.dateOfAdmission) "DateOfAdmissionId2"
    , DateInput "Secondary Date of Discharge" Optional (defaultString model.sfData.dateOfDischarge) "DateOfDischargeId2"
    ]


type Msg
    = Save
    | SaveCompleted (Result Http.Error String)
    | Cancel
    | UpdateHospitilizationsInitData SyncfusionData
    | UpdatePatientReported Bool
    | UpdateChiefComplaint String
    | UpdateDischargeRecommendations String


update : Msg -> Model -> Int -> ( Model, Cmd Msg )
update msg model patientId =
    let
        updateAddNew t =
            t ! [ setUnsavedChanges True ]
    in
        case msg of
            Save ->
                if List.length (getValidationErrors (formInputs model)) > 0 then
                    { model | showValidationErrors = True } ! []
                else
                    model ! [ saveForm model patientId SaveCompleted, setUnsavedChanges False ]

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

            UpdateHospitilizationsInitData hospitilizationsInitData ->
                { model | sfData = hospitilizationsInitData } ! []

            UpdatePatientReported bool ->
                updateAddNew { model | patientReported = bool }

            UpdateChiefComplaint str ->
                updateAddNew { model | chiefComplaint = str }

            UpdateDischargeRecommendations str ->
                updateAddNew { model | dischargeRecommendations = str }


encodeRecord : Model -> Int -> Encode.Value
encodeRecord newRecord patientId =
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


saveFormRequest : Model -> Int -> Http.Request String
saveFormRequest model patientId =
    Http.request
        { body = encodeRecord model patientId |> Http.jsonBody
        , expect = Http.expectString
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = "/People/AddEditHospitilization"
        , withCredentials = False
        }


saveForm : Model -> Int -> (Result Http.Error String -> msg) -> Cmd msg
saveForm model patientId saveCompleted =
    Http.send saveCompleted (saveFormRequest model patientId)


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


emptyModel : Maybe HospitilizationsRow -> Model
emptyModel hospitilizationRow =
    case hospitilizationRow of
        Just t ->
            { sfData = emptySfData
            , patientReported = t.patientReported
            , chiefComplaint = t.chiefComplaint
            , dischargeRecommendations = t.dischargeRecommendations
            , showValidationErrors = False
            }

        Nothing ->
            { sfData = emptySfData
            , patientReported = False
            , chiefComplaint = ""
            , dischargeRecommendations = ""
            , showValidationErrors = False
            }


emptySfData : SyncfusionData
emptySfData =
    { id = Nothing
    , facilities = []
    , hospitilizationServiceTypes = []
    , hospitalizationDischargePhysicians = []
    , facilityId = Nothing
    , admitDiagnosisId = Nothing
    , dischargeDiagnosisId = Nothing
    , facilityId2 = Nothing
    , hospitalServiceTypeId = Nothing
    , dischargePhysicianId = Nothing
    , dateOfAdmission = Nothing
    , dateOfDischarge = Nothing
    , dateOfAdmission2 = Nothing
    , dateOfDischarge2 = Nothing
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
