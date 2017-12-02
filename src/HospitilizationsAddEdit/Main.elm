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


port initHospitilizations : InitHospitilizationsAddNew -> Cmd msg


port updateHospFacility : (DropDownItem -> msg) -> Sub msg


port updateHospFacility2 : (DropDownItem -> msg) -> Sub msg


port updateHospDateOfAdmission : (Maybe String -> msg) -> Sub msg


port updateHospDateOfDischarge : (Maybe String -> msg) -> Sub msg


port updateHospHospitalServiceType : (DropDownItem -> msg) -> Sub msg


port updateAdmitDiagnosis : (String -> msg) -> Sub msg


port updateDischargeDiagnosis : (String -> msg) -> Sub msg


port updateHospDischargePhysician : (DropDownItem -> msg) -> Sub msg


port updateHospDateOfAdmission2 : (Maybe String -> msg) -> Sub msg


port updateHospDateOfDischarge2 : (Maybe String -> msg) -> Sub msg


port addHospNewFacility : Maybe String -> Cmd msg


port addHospNewPhysician : Maybe String -> Cmd msg


init : AddEditDataSource -> Maybe Int -> RecordAddNewInitData -> Cmd Msg
init addEditDataSource hospitilizationId recordAddNewInitData =
    initHospitilizations (getHospitilizationMsg addEditDataSource hospitilizationId recordAddNewInitData)


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ updateHospFacility UpdateFacility
        , updateHospDateOfAdmission UpdateDateOfAdmission
        , updateHospDateOfDischarge UpdateDateOfDischarge
        , updateHospFacility2 UpdateFacility2
        , updateHospDateOfDischarge UpdateDateOfDischarge
        , updateHospHospitalServiceType UpdateHospitalServiceType
        , updateAdmitDiagnosis UpdateAdmitDiagnosis
        , updateDischargeDiagnosis UpdateDischargeDiagnosis
        , updateHospDischargePhysician UpdateDischargePhysician
        , updateHospDateOfAdmission2 UpdateDateOfAdmission2
        , updateHospDateOfDischarge2 UpdateDateOfDischarge2
        ]


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

            UpdateFacility dropDownItem ->
                updateAddNew { model | facilityId = dropDownItem.id, facilityText = dropDownItem.name }

            AddNewFacility ->
                model ! [ addHospNewFacility Nothing ]

            AddNewPhysician ->
                model ! [ addHospNewPhysician Nothing ]

            UpdateHospitilization dropDownItem ->
                updateAddNew { model | hospitalizationId = dropDownItem.id, hospitalizationText = dropDownItem.name }

            UpdatePatientReported bool ->
                updateAddNew { model | patientReported = bool }

            UpdateDateOfAdmission str ->
                updateAddNew { model | dateOfAdmission = str }

            UpdateDateOfDischarge str ->
                updateAddNew { model | dateOfDischarge = str }

            UpdateHospitalServiceType dropDownItem ->
                updateAddNew { model | hospitalServiceTypeId = dropDownItem.id, hospitalServiceTypeText = dropDownItem.name }

            UpdateChiefComplaint str ->
                updateAddNew { model | chiefComplaint = str }

            UpdateAdmitDiagnosis str ->
                updateAddNew { model | admitDiagnosisId = maybeStringToInt str }

            UpdateDischargeDiagnosis str ->
                updateAddNew { model | dischargeDiagnosisId = maybeStringToInt str }

            UpdateDischargeRecommendations str ->
                updateAddNew { model | dischargeRecommendations = str }

            UpdateDischargePhysician dropDownItem ->
                updateAddNew { model | dischargePhysicianId = dropDownItem.id, dischargePhysicianText = dropDownItem.name }

            UpdateFacility2 dropDownItem ->
                updateAddNew { model | facilityId2 = dropDownItem.id, facilityText2 = dropDownItem.name }

            UpdateDateOfAdmission2 str ->
                updateAddNew { model | dateOfAdmission2 = str }

            UpdateDateOfDischarge2 str ->
                updateAddNew { model | dateOfDischarge2 = str }


formInputs : Model -> List ( String, RequiredType, InputControlType Msg )
formInputs newRecord =
    [ ( "Patient Reported", Optional, CheckInput newRecord.patientReported UpdatePatientReported )
    , ( "Facility Name", Required, DropInputWithButton newRecord.facilityId "FacilityId" AddNewFacility "Add New Facility" )
    , ( "Date of Admission", Required, DateInput (defaultString newRecord.dateOfAdmission) "DateOfAdmissionId" UpdateDateOfAdmission )
    , ( "Date of Discharge", Required, DateInput (defaultString newRecord.dateOfDischarge) "DateOfDischargeId" UpdateDateOfDischarge )
    , ( "Hospital Service Type", Required, DropInput newRecord.hospitalServiceTypeId "HospitalServiceTypeId" )
    , ( "Chief Complaint", Required, AreaInput newRecord.chiefComplaint UpdateChiefComplaint )
    , ( "Admit Diagnosis", Required, KnockInput "HospitalizationAdmitProblemSelection" )
    , ( "Discharge Diagnosis", Required, KnockInput "HospitalizationDischargeProblemSelection" )
    , ( "Discharge Recommendations", Required, TextInput newRecord.dischargeRecommendations UpdateDischargeRecommendations )
    , ( "Discharge Physician", Optional, DropInputWithButton newRecord.dischargePhysicianId "DischargePhysicianId" AddNewPhysician "New Provider" )
    , ( "Secondary Facility Name", Optional, DropInputWithButton newRecord.facilityId2 "FacilityId2" AddNewFacility "Add New Facility" )
    , ( "Secondary Date of Admission", Optional, DateInput (defaultString newRecord.dateOfAdmission) "DateOfAdmissionId2" UpdateDateOfAdmission2 )
    , ( "Secondary Date of Discharge", Optional, DateInput (defaultString newRecord.dateOfDischarge) "DateOfDischargeId2" UpdateDateOfDischarge2 )
    ]
