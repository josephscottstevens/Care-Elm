port module HospitilizationsAddEdit.Main exposing (..)

import HospitilizationsAddEdit.Functions exposing (..)
import HospitilizationsAddEdit.Types exposing (..)
import Html exposing (Html, text, div, button)
import Html.Attributes exposing (class, id, value, type_)
import Html.Events exposing (onClick)
import Common.Html exposing (..)
import Common.Types exposing (..)
import Common.Functions exposing (..)
import Common.Routes exposing (navHospitilizations)
import Ports exposing (setUnsavedChanges)


port initHospitilizations : HospitilizationsInitData -> Cmd msg


port updateHospitilizations : (HospitilizationsInitData -> msg) -> Sub msg


init : Maybe AddEditDataSource -> Maybe HospitilizationsRow -> Int -> ( Model, Cmd Msg )
init addEditDataSource hospitilizationsRow patientId =
    let
        model =
            emptyModel patientId
    in
        case addEditDataSource of
            Just t ->
                updateModel model hospitilizationsRow ! [ initHospitilizations (getHospitilizationsInitData t hospitilizationsRow) ]

            Nothing ->
                model ! [ displayErrorMessage "Cannot load Hospitilizations without a datasource!" ]


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
            , makeControls (formInputs model)
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
                        model ! [ displaySuccessMessage "Save completed successfully!", navHospitilizations ]

            SaveCompleted (Err t) ->
                model ! [ displayErrorMessage (toString t) ]

            Cancel ->
                model ! [ setUnsavedChanges False, navHospitilizations ]

            UpdateHospitilizationsInitData hospitilizationsInitData ->
                { model | initData = hospitilizationsInitData } ! []

            UpdatePatientReported bool ->
                updateAddNew { model | patientReported = bool }

            UpdateChiefComplaint str ->
                updateAddNew { model | chiefComplaint = str }

            UpdateDischargeRecommendations str ->
                updateAddNew { model | dischargeRecommendations = str }


formInputs : Model -> List ( String, RequiredType, InputControlType Msg )
formInputs model =
    [ ( "Patient Reported", Optional, CheckInput model.patientReported UpdatePatientReported )
    , ( "Facility Name", Required, DropInputWithButton model.initData.facilityId "FacilityId" "Add New Facility" )
    , ( "Date of Admission", Required, DateInput (defaultString model.initData.dateOfAdmission) "DateOfAdmissionId" )
    , ( "Date of Discharge", Required, DateInput (defaultString model.initData.dateOfDischarge) "DateOfDischargeId" )
    , ( "Hospital Service Type", Required, DropInput model.initData.hospitalServiceTypeId "HospitalServiceTypeId" )
    , ( "Chief Complaint", Required, AreaInput model.chiefComplaint UpdateChiefComplaint )
    , ( "Admit Diagnosis", Required, KnockInput "HospitalizationAdmitProblemSelection" )
    , ( "Discharge Diagnosis", Required, KnockInput "HospitalizationDischargeProblemSelection" )
    , ( "Discharge Recommendations", Required, TextInput model.dischargeRecommendations UpdateDischargeRecommendations )
    , ( "Discharge Physician", Optional, DropInputWithButton model.initData.dischargePhysicianId "DischargePhysicianId" "New Provider" )
    , ( "Secondary Facility Name", Optional, DropInputWithButton model.initData.facilityId2 "FacilityId2" "Add New Facility" )
    , ( "Secondary Date of Admission", Optional, DateInput (defaultString model.initData.dateOfAdmission) "DateOfAdmissionId2" )
    , ( "Secondary Date of Discharge", Optional, DateInput (defaultString model.initData.dateOfDischarge) "DateOfDischargeId2" )
    ]
