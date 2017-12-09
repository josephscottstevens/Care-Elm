port module HospitilizationsAddEdit.Main exposing (..)

import HospitilizationsAddEdit.Functions exposing (..)
import HospitilizationsAddEdit.Types exposing (..)
import Html exposing (Html, text, div, button)
import Html.Attributes exposing (class, id, value, type_)
import Html.Events exposing (onClick)
import Common.Html exposing (..)
import Common.Types exposing (..)
import Common.Functions exposing (..)
import Route
import Ports exposing (setUnsavedChanges)


port initHospitilizations : HospitilizationsInitData -> Cmd msg


port updateHospitilizations : (HospitilizationsInitData -> msg) -> Sub msg


init : AddEditDataSource -> Maybe HospitilizationsRow -> Cmd Msg
init addEditDataSource hospitilizationsRow =
    initHospitilizations (getHospitilizationsInitData addEditDataSource hospitilizationsRow)


subscriptions : Sub Msg
subscriptions =
    updateHospitilizations UpdateHospitilizationsInitData


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateAddNew t =
            t ! [ setUnsavedChanges True ]
    in
        case msg of
            Save ->
                if List.length (getValidationErrors (formInputs model)) > 0 then
                    { model | showValidationErrors = True } ! []
                else
                    model ! [ saveForm model, setUnsavedChanges False ]

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
                { model | initData = hospitilizationsInitData } ! []

            UpdatePatientReported bool ->
                updateAddNew { model | patientReported = bool }

            UpdateChiefComplaint str ->
                updateAddNew { model | chiefComplaint = str }

            UpdateDischargeRecommendations str ->
                updateAddNew { model | dischargeRecommendations = str }


formInputs : Model -> List (InputControlType Msg)
formInputs model =
    [ CheckInput "Patient Reported" Optional model.patientReported UpdatePatientReported
    , DropInputWithButton "Facility Name" Required model.initData.facilityId "FacilityId" "Add New Facility"
    , DateInput "Date of Admission" Required (defaultString model.initData.dateOfAdmission) "DateOfAdmissionId"
    , DateInput "Date of Discharge" Required (defaultString model.initData.dateOfDischarge) "DateOfDischargeId"
    , DropInput "Hospital Service Type" Required model.initData.hospitalServiceTypeId "HospitalServiceTypeId"
    , AreaInput "Chief Complaint" Required model.chiefComplaint UpdateChiefComplaint
    , KnockInput "Admit Diagnosis" Required "HospitalizationAdmitProblemSelection"
    , KnockInput "Discharge Diagnosis" Required "HospitalizationDischargeProblemSelection"
    , TextInput "Discharge Recommendations" Required model.dischargeRecommendations UpdateDischargeRecommendations
    , DropInputWithButton "Discharge Physician" Optional model.initData.dischargePhysicianId "DischargePhysicianId" "New Provider"
    , DropInputWithButton "Secondary Facility Name" Optional model.initData.facilityId2 "FacilityId2" "Add New Facility"
    , DateInput "Secondary Date of Admission" Optional (defaultString model.initData.dateOfAdmission) "DateOfAdmissionId2"
    , DateInput "Secondary Date of Discharge" Optional (defaultString model.initData.dateOfDischarge) "DateOfDischargeId2"
    ]
