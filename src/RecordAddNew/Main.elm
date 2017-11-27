port module RecordAddNew.Main exposing (..)

import RecordAddNew.Functions exposing (..)
import RecordAddNew.Types exposing (..)
import Html exposing (Html, text, div, button)
import Html.Attributes exposing (class, id, value, type_)
import Html.Events exposing (onClick)
import Common.Html exposing (..)
import Common.Types exposing (..)
import Common.Functions exposing (..)


port resetUpdate : Maybe Int -> Cmd msg


port addNewFacility : Maybe String -> Cmd msg


port addNewPhysician : Maybe String -> Cmd msg


port initSyncfusionControls : SyncfusionMessage -> Cmd msg


port setUnsavedChanges : Bool -> Cmd msg


port loadDataSourceComplete : (AddEditDataSource -> msg) -> Sub msg


port resetUpdateComplete : (Maybe Int -> msg) -> Sub msg


port updateFacility : (DropDownItem -> msg) -> Sub msg


port updateCategory : (DropDownItem -> msg) -> Sub msg


port updateTimeVisit : (Maybe String -> msg) -> Sub msg


port updateTimeAcc : (Maybe String -> msg) -> Sub msg


port updateFileName : (String -> msg) -> Sub msg


port updateReportDate : (Maybe String -> msg) -> Sub msg


port updateRecordingDate : (Maybe String -> msg) -> Sub msg


port updateUser : (DropDownItem -> msg) -> Sub msg


port updateTask : (DropDownItem -> msg) -> Sub msg



-- Hospitilizations


port updateFacility2 : (DropDownItem -> msg) -> Sub msg


port updateDateOfAdmission : (Maybe String -> msg) -> Sub msg


port updateDateOfDischarge : (Maybe String -> msg) -> Sub msg


port updateHospitalServiceType : (DropDownItem -> msg) -> Sub msg


port updateDischargePhysician : (DropDownItem -> msg) -> Sub msg


init : Flags -> AddEditDataSource -> Cmd Msg
init flags addEditDataSource =
    initSyncfusionControls (getSyncfusionMessage addEditDataSource flags.recordTypeId False)


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ updateFacility UpdateFacility
        , updateCategory UpdateRecordType
        , updateTimeVisit UpdateTimeVisit
        , updateTimeAcc UpdateTimeAcc
        , updateFileName UpdateFileName
        , updateReportDate UpdateReportDate
        , updateRecordingDate UpdateRecordingDate
        , updateUser UpdateUser
        , updateTask UpdateTask
        , resetUpdateComplete ResetUpdateComplete
        , loadDataSourceComplete LoadDataSource

        -- Hospitilizations
        , updateFacility2 UpdateFacility2
        , updateDateOfAdmission UpdateDateOfAdmission
        , updateDateOfDischarge UpdateDateOfDischarge
        , updateHospitalServiceType UpdateHospitalServiceType
        , updateDischargePhysician UpdateDischargePhysician
        ]


view : Model -> Html Msg
view model =
    case model.state of
        AddEdit ->
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

        Limbo ->
            div [] []

        Error str ->
            div [] [ text str ]


update : Msg -> Model -> ( ( Model, Cmd Msg ), Bool )
update msg model =
    let
        updateAddNew t =
            ( t ! [ setUnsavedChanges True ], False )
    in
        case msg of
            AddNewFacility ->
                ( model ! [ addNewFacility Nothing ], False )

            AddNewPhysician ->
                ( model ! [ addNewPhysician Nothing ], False )

            Save ->
                if List.length (getValidationErrors (formInputs model)) > 0 then
                    ( { model | showValidationErrors = True } ! [], False )
                else
                    ( model ! [ saveForm model, setUnsavedChanges False ], False )

            SaveCompleted (Ok responseMsg) ->
                case getResponseError responseMsg of
                    Just t ->
                        ( model ! [ displayErrorMessage t ], False )

                    Nothing ->
                        ( model ! [ displaySuccessMessage "Save completed successfully!" ], True )

            SaveCompleted (Err httpError) ->
                ( { model | state = Error (toString httpError) } ! [ setLoadingStatus False ], False )

            Cancel ->
                ( model ! [ setUnsavedChanges False ], True )

            ResetUpdateComplete dropDownId ->
                case model.addEditDataSource of
                    Just t ->
                        ( { model | state = AddEdit } ! [ initSyncfusionControls (getSyncfusionMessage t model.recordTypeId True) ], False )

                    Nothing ->
                        Debug.crash "error, no drops"

            LoadDataSource addEditDataSource ->
                ( { model | addEditDataSource = Just addEditDataSource } ! [ setLoadingStatus False ], False )

            UpdateRecordType dropDownItem ->
                if model.recordTypeId == dropDownItem.id then
                    ( model ! [], False )
                else
                    ( { model | state = Limbo, recordTypeId = dropDownItem.id } ! [ resetUpdate dropDownItem.id, setLoadingStatus True ], False )

            UpdateTitle str ->
                updateAddNew { model | title = str }

            UpdateSpecialty str ->
                updateAddNew { model | specialty = str }

            UpdateProvider str ->
                updateAddNew { model | provider = str }

            UpdateTimeVisit str ->
                updateAddNew { model | timeVisit = str }

            UpdateTimeAcc str ->
                updateAddNew { model | timeAcc = str }

            UpdateFileName str ->
                updateAddNew { model | fileName = str }

            UpdateComments str ->
                updateAddNew { model | comments = str }

            UpdateFacility dropDownItem ->
                updateAddNew { model | facilityId = dropDownItem.id, facilityText = dropDownItem.name }

            UpdateReportDate str ->
                updateAddNew { model | reportDate = str }

            UpdateCallSid str ->
                updateAddNew { model | callSid = str }

            UpdateRecordingSid str ->
                updateAddNew { model | recording = str }

            UpdateDuration str ->
                updateAddNew { model | duration = defaultIntStr str }

            UpdateRecordingDate str ->
                updateAddNew { model | recordingDate = str }

            UpdateUser dropDownItem ->
                updateAddNew { model | userId = dropDownItem.id, userText = dropDownItem.name }

            UpdateTask dropDownItem ->
                updateAddNew { model | taskId = dropDownItem.id, taskText = dropDownItem.name }

            -- Hospitilizations
            UpdatePatientReported bool ->
                updateAddNew { model | patientReported = bool }

            UpdateFacility2 dropDownItem ->
                updateAddNew { model | facilityId2 = dropDownItem.id, facilityText2 = dropDownItem.name }

            UpdateDateOfAdmission str ->
                updateAddNew { model | dateOfAdmission = str }

            UpdateDateOfDischarge str ->
                updateAddNew { model | dateOfDischarge = str }

            UpdateHospitalServiceType dropDownItem ->
                updateAddNew { model | hospitalServiceTypeId = dropDownItem.id, hospitalServiceTypeText = dropDownItem.name }

            UpdateDischargeRecommendations str ->
                updateAddNew { model | dischargeRecommendations = str }

            UpdateDischargePhysician dropDownItem ->
                updateAddNew { model | dischargePhysicianId = dropDownItem.id, dischargePhysicianText = dropDownItem.name }


formInputs : Model -> List ( String, RequiredType, InputControlType Msg )
formInputs newRecord =
    let
        recordType =
            getRecordType newRecord.recordTypeId

        firstColumns =
            [ ( "Facility", Required, DropInput newRecord.facilityId "FacilityId" )
            , ( "Category", Required, DropInput newRecord.recordTypeId "CategoryId" )
            ]

        lastColumns =
            [ ( "Comments", Required, AreaInput newRecord.comments UpdateComments )
            , ( "Upload Record File", Required, FileInput newRecord.fileName )
            ]

        defaultFields =
            firstColumns
                ++ [ ( "Date of Visit", Required, DateInput (defaultString newRecord.timeVisit) "TimeVisitId" UpdateTimeVisit )
                   , ( "Doctor of Visit", Optional, TextInput newRecord.provider UpdateProvider )
                   , ( "Specialty of Visit", Optional, TextInput newRecord.specialty UpdateSpecialty )
                   ]
                ++ lastColumns

        columns =
            case recordType of
                PrimaryCare ->
                    defaultFields

                Specialty ->
                    defaultFields

                Labs ->
                    firstColumns
                        ++ [ ( "Date/Time of Labs Collected", Required, DateInput (defaultString newRecord.timeVisit) "TimeVisitId" UpdateTimeVisit )
                           , ( "Date/Time of Labs Accessioned", Required, DateInput (defaultString newRecord.timeAcc) "TimeAccId" UpdateTimeAcc )
                           , ( "Name of Lab", Optional, TextInput newRecord.title UpdateTitle )
                           , ( "Provider of Lab", Optional, TextInput newRecord.provider UpdateProvider )
                           ]
                        ++ lastColumns

                Radiology ->
                    firstColumns
                        ++ [ ( "Date/Time of Study was done", Required, DateInput (defaultString newRecord.timeVisit) "TimeVisitId" UpdateTimeVisit )
                           , ( "Date/Time of Study Accessioned", Required, DateInput (defaultString newRecord.timeAcc) "TimeAccId" UpdateTimeAcc )
                           , ( "Name of Study", Optional, TextInput newRecord.title UpdateTitle )
                           , ( "Provider of Study", Optional, TextInput newRecord.provider UpdateProvider )
                           ]
                        ++ lastColumns

                Misc ->
                    defaultFields

                Legal ->
                    firstColumns
                        ++ [ ( "Title", Optional, TextInput newRecord.title UpdateTitle ) ]
                        ++ lastColumns

                Hospitalizations ->
                    case newRecord.hospitalizationId of
                        Just _ ->
                            []

                        Nothing ->
                            [ ( "Patient Reported", Optional, CheckInput UpdatePatientReported )
                            , ( "Facility", Required, DropInputWithButton newRecord.facilityId "FacilityId" AddNewFacility "Add New Facility" )
                            , ( "Date of Admission", Required, DateInput (defaultString newRecord.dateOfAdmission) "DateOfAdmissionId" UpdateDateOfAdmission )
                            , ( "Date of Discharge", Required, DateInput (defaultString newRecord.dateOfDischarge) "DateOfDischargeId" UpdateDateOfDischarge )
                            , ( "Hospital Service Type", Required, DropInput newRecord.hospitalServiceTypeId "HospitalServiceTypeId" )
                            , ( "Chief Complaint", Required, AreaInput newRecord.comments UpdateComments )
                            , ( "Admit Diagnosis", Required, KnockInput "HospitalizationAdmitProblemSelection" )
                            , ( "Discharge Diagnosis", Required, KnockInput "HospitalizationDischargeProblemSelection" )
                            , ( "Discharge Recommendations", Required, TextInput newRecord.dischargeRecommendations UpdateDischargeRecommendations )
                            , ( "Discharge Physician", Required, DropInputWithButton newRecord.dischargePhysicianId "DischargePhysicianId" AddNewPhysician "New Provider" )
                            , ( "Secondary Facility Name", Required, DropInputWithButton newRecord.facilityId2 "FacilityId2" AddNewFacility "Add New Facility" )
                            , ( "Secondary Date of Admission", Required, DateInput (defaultString newRecord.dateOfAdmission) "DateOfAdmissionId" UpdateDateOfAdmission )
                            , ( "Secondary Date of Discharge", Required, DateInput (defaultString newRecord.dateOfDischarge) "DateOfDischargeId" UpdateDateOfDischarge )
                            ]

                CallRecordings ->
                    firstColumns
                        ++ [ ( "Call Sid", Required, TextInput newRecord.callSid UpdateCallSid )
                           , ( "Recording Sid", Required, TextInput newRecord.recording UpdateRecordingSid )
                           , ( "Duration", Required, NumrInput newRecord.duration UpdateDuration )
                           , ( "Recording Date", Required, DateInput (defaultString newRecord.recordingDate) "RecordingDateId" UpdateRecordingDate )
                           , ( "User", Required, DropInput newRecord.userId "UserId" )
                           , ( "Task", Optional, DropInput newRecord.taskId "TaskId" )
                           ]

                PreviousHistories ->
                    firstColumns
                        ++ [ ( "Report Date", Required, DateInput (defaultString newRecord.reportDate) "ReportDateId" UpdateReportDate )
                           , ( "Upload Record File", Required, FileInput newRecord.fileName )
                           ]

                Enrollment ->
                    firstColumns
                        ++ [ ( "Title", Optional, TextInput newRecord.title UpdateTitle )
                           ]
                        ++ lastColumns
    in
        columns
