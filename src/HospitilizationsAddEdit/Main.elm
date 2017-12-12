port module HospitilizationsAddEdit.Main exposing (Msg, subscriptions, init, update, view)

import HospitilizationsAddEdit.Functions exposing (getHospitilizationsInitData, saveForm)
import HospitilizationsAddEdit.Types exposing (Model, SyncfusionData)
import Html exposing (Html, text, div, button)
import Html.Attributes exposing (class, id, value, type_)
import Html.Events exposing (onClick)
import Common.Html exposing (..)
import Common.Types exposing (..)
import Common.Functions exposing (..)
import Common.Ports exposing (setUnsavedChanges)
import Route
import Http


port initHospitilizations : SyncfusionData -> Cmd msg


port updateHospitilizations : (SyncfusionData -> msg) -> Sub msg


subscriptions : Sub Msg
subscriptions =
    updateHospitilizations UpdateHospitilizationsInitData


init : AddEditDataSource -> Maybe HospitilizationsRow -> Cmd Msg
init addEditDataSource hospitilizationsRow =
    initHospitilizations (getHospitilizationsInitData addEditDataSource hospitilizationsRow)


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
                case getResponseError responseMsg of
                    Just t ->
                        model ! [ displayErrorMessage t ]

                    Nothing ->
                        model ! [ displaySuccessMessage "Save completed successfully!", Route.modifyUrl Route.Hospitilizations ]

            SaveCompleted (Err t) ->
                model ! [ displayErrorMessage (toString t) ]

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

        saveBtnClass =
            class "btn btn-sm btn-success margin-left-5 pull-right"
    in
        div [ class "form-horizontal" ]
            [ validationErrorsDiv
            , makeControls defaultConfig (formInputs model)
            , div [ class "form-group" ]
                [ div [ class fullWidth ]
                    [ button [ type_ "button", id "Save", value "AddNewRecord", onClick Save, saveBtnClass ] [ text "Save" ]
                    , button [ type_ "button", onClick Cancel, class "btn btn-sm btn-default pull-right" ] [ text "Cancel" ]
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
