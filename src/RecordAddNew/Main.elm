port module RecordAddNew.Main exposing (..)

import RecordAddNew.Functions exposing (..)
import RecordAddNew.Types exposing (..)
import Html exposing (Html, text, div, button)
import Html.Attributes exposing (class, id, type_, value)
import Html.Events exposing (onClick, onFocus)
import Table exposing (..)
import Utils.CommonGrid exposing (..)
import Utils.CommonHtml exposing (..)
import Utils.CommonTypes exposing (..)
import Utils.CommonFunctions exposing (..)


port resetUpdate : Maybe Int -> Cmd msg


port addNewFacility : Maybe String -> Cmd msg


port addNewPhysician : Maybe String -> Cmd msg


port initSyncfusionControls : AddEditDataSource -> Cmd msg


port setUnsavedChanges : Bool -> Cmd msg


port resetUpdateComplete : (String -> msg) -> Sub msg


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



-- my init function
--  AddNewStart ->
--             { model | state = AddNew (getNewRecord model) } ! [ initSyncfusionControls (getSyncFusionMessage model False) ]
-- Okay... so before I can do this, I need dropdowns, and some fields, these aren't really apart of my Model, these are extra fields that should be loaded before I start
-- init : Flags -> Cmd Msg
-- init flag =
--     initSyncfusionControls (getSyncFusionMessage model False)


subscriptions : Model -> Sub Msg
subscriptions model =
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

        -- Hospitilizations
        , updateFacility2 UpdateFacility2
        , updateDateOfAdmission UpdateDateOfAdmission
        , updateDateOfDischarge UpdateDateOfDischarge
        , updateHospitalServiceType UpdateHospitalServiceType
        , updateDischargePhysician UpdateDischargePhysician
        ]



--        , resetUpdateComplete ResetAddNew


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

        footerControls =
            div [ class "form-group" ]
                [ div [ class fullWidth ]
                    [--button [ type_ "button", id "Save", value "AddNewRecord", onClick (Save model), saveBtnClass ] [ text "Save" ]
                     -- , button [ type_ "button", onClick Cancel, class "btn btn-sm btn-default pull-right" ] [ text "Cancel" ]
                    ]
                ]
    in
        div
            [ class "form-horizontal" ]
            [ validationErrorsDiv
            , makeControls (formInputs model)
            , footerControls
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateAddNew t =
            t ! [ setUnsavedChanges True ]
    in
        case msg of
            -- Save newRecord ->
            --     if List.length (getValidationErrors formInputs ) > 0 then
            --         updateAddNew { newRecord | showValidationErrors = True }
            --     else
            --         model ! [ saveForm newRecord, setUnsavedChanges False ]
            -- SaveCompleted (Ok responseMsg) ->
            --     case getResponseError responseMsg of
            --         Just t ->
            --             model ! [ getRecords model.patientId model.recordTypeId Load, displayErrorMessage t ]
            --         Nothing ->
            --             model ! [ getRecords model.patientId model.recordTypeId Load, displaySuccessMessage "Save completed successfully!" ]
            -- SaveCompleted (Err httpError) ->
            --     { model | state = Error (toString httpError) } ! [ setLoadingStatus False ]
            AddNewFacility ->
                model ! [ addNewFacility Nothing ]

            AddNewPhysician ->
                model ! [ addNewPhysician Nothing ]

            -- Cancel ->
            --     { model | state = Grid } ! [ setUnsavedChanges False ]
            -- ResetAddNew _ ->
            --     { model | state = AddEdit (getNewRecord model) } ! [ initSyncfusionControls (getSyncFusionMessage model True) ]
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

            UpdateRecordType dropDownItem ->
                if model.recordTypeId == dropDownItem.id then
                    model ! []
                else
                    { model | state = Limbo, recordTypeId = dropDownItem.id } ! [ resetUpdate dropDownItem.id, setLoadingStatus True ]


formInputs : Model -> List ( String, RequiredType, InputControlType Msg )
formInputs newRecord =
    let
        recordType =
            getRecordType newRecord.recordTypeId

        defaultFields =
            [ ( "Date of Visit", Required, DateInput (defaultString newRecord.timeVisit) "TimeVisitId" UpdateTimeVisit )
            , ( "Doctor of Visit", Optional, TextInput newRecord.provider UpdateProvider )
            , ( "Specialty of Visit", Optional, TextInput newRecord.specialty UpdateSpecialty )
            , ( "Comments", Required, AreaInput newRecord.comments UpdateComments )
            , ( "Upload Record File", Required, FileInput newRecord.fileName )
            ]

        firstColumns =
            [ ( "Facility", Required, DropInput newRecord.facilityId "FacilityId" )
            , ( "Category", Required, DropInput newRecord.recordTypeId "CategoryId" )
            ]

        lastColumns =
            case recordType of
                PrimaryCare ->
                    defaultFields

                Specialty ->
                    defaultFields

                Labs ->
                    [ ( "Date/Time of Labs Collected", Required, DateInput (defaultString newRecord.timeVisit) "TimeVisitId" UpdateTimeVisit )
                    , ( "Date/Time of Labs Accessioned", Required, DateInput (defaultString newRecord.timeAcc) "TimeAccId" UpdateTimeAcc )
                    , ( "Name of Lab", Optional, TextInput newRecord.title UpdateTitle )
                    , ( "Provider of Lab", Optional, TextInput newRecord.provider UpdateProvider )
                    , ( "Comments", Required, AreaInput newRecord.comments UpdateComments )
                    , ( "Upload Record File", Required, FileInput newRecord.fileName )
                    ]

                Radiology ->
                    [ ( "Date/Time of Study was done", Required, DateInput (defaultString newRecord.timeVisit) "TimeVisitId" UpdateTimeVisit )
                    , ( "Date/Time of Study Accessioned", Required, DateInput (defaultString newRecord.timeAcc) "TimeAccId" UpdateTimeAcc )
                    , ( "Name of Study", Optional, TextInput newRecord.title UpdateTitle )
                    , ( "Provider of Study", Optional, TextInput newRecord.provider UpdateProvider )
                    , ( "Comments", Required, TextInput newRecord.comments UpdateComments )
                    , ( "Upload Record File", Required, FileInput newRecord.fileName )
                    ]

                Misc ->
                    defaultFields

                Legal ->
                    [ ( "Title", Optional, TextInput newRecord.title UpdateTitle )
                    , ( "Comments", Required, AreaInput newRecord.comments UpdateComments )
                    , ( "Upload Record File", Required, FileInput newRecord.fileName )
                    ]

                Hospitalizations ->
                    case newRecord.hospitalizationId of
                        Just t ->
                            []

                        Nothing ->
                            [ ( "Facility", Required, DropInputWithButton newRecord.facilityId2 "FacilityId2" AddNewFacility "Add New Facility" )
                            , ( "Date of Admission", Required, DateInput (defaultString newRecord.dateOfAdmission) "DateOfAdmissionId" UpdateDateOfAdmission )
                            , ( "Date of Discharge", Required, DateInput (defaultString newRecord.dateOfDischarge) "DateOfDischargeId" UpdateDateOfDischarge )
                            , ( "Hospital Service Type", Required, DropInput newRecord.hospitalServiceTypeId "HospitalServiceTypeId" )
                            , ( "Discharge Recommendations", Required, TextInput newRecord.dischargeRecommendations UpdateDischargeRecommendations )
                            , ( "Discharge Physician", Required, DropInputWithButton newRecord.dischargePhysicianId "DischargePhysicianId" AddNewPhysician "Add New Physician" )
                            , ( "Comments", Required, AreaInput newRecord.comments UpdateComments )
                            , ( "Upload Record File", Required, FileInput newRecord.fileName )
                            ]

                CallRecordings ->
                    [ ( "Call Sid", Required, TextInput newRecord.callSid UpdateCallSid )
                    , ( "Recording Sid", Required, TextInput newRecord.recording UpdateRecordingSid )
                    , ( "Duration", Required, NumrInput newRecord.duration UpdateDuration )
                    , ( "Recording Date", Required, DateInput (defaultString newRecord.recordingDate) "RecordingDateId" UpdateRecordingDate )
                    , ( "User", Required, DropInput newRecord.userId "UserId" )
                    , ( "Task", Optional, DropInput newRecord.taskId "TaskId" )
                    ]

                PreviousHistories ->
                    [ ( "Report Date", Required, DateInput (defaultString newRecord.reportDate) "ReportDateId" UpdateReportDate )
                    , ( "Upload Record File", Required, FileInput newRecord.fileName )
                    ]

                Enrollment ->
                    [ ( "Title", Optional, TextInput newRecord.title UpdateTitle )
                    , ( "Comments", Required, AreaInput newRecord.comments UpdateComments )
                    , ( "Upload Record File", Required, FileInput newRecord.fileName )
                    ]
    in
        List.append firstColumns lastColumns
