module Records.Functions exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Http
import Table
import Records.Types exposing (..)
import Common.Types exposing (..)
import Common.Functions exposing (..)
import String exposing (toLower)


-- Http helper functions


decodeRecordRow : Decoder RecordRow
decodeRecordRow =
    decode RecordRow
        |> required "Id" Decode.int
        |> required "Date" (maybe Decode.string)
        |> required "Specialty" (maybe Decode.string)
        |> required "Comments" (maybe Decode.string)
        |> required "TransferedTo" (maybe Decode.string)
        |> required "TransferedOn" (maybe Decode.string)
        |> required "PatientId" Decode.int
        |> required "Title" (maybe Decode.string)
        |> required "DateAccessed" (maybe Decode.string)
        |> required "Provider" (maybe Decode.string)
        |> required "PatientName" (maybe Decode.string)
        |> required "RecordType" (maybe Decode.string)
        |> required "DateOfAdmission" (maybe Decode.string)
        |> required "DateOfDischarge" (maybe Decode.string)
        |> required "DischargePhysician" (maybe Decode.string)
        |> required "DischargeDiagnosis" (maybe Decode.string)
        |> required "HospitalizationServiceType" (maybe Decode.string)
        |> required "HospitalizationId" (maybe Decode.int)
        |> required "ReportDate" (maybe Decode.string)
        |> required "FileName" (maybe Decode.string)
        |> required "CanTransfer" Decode.bool
        |> required "Facility" (maybe Decode.string)
        |> required "FacilityFax" (maybe Decode.string)
        |> required "Recommendations" (maybe Decode.string)
        |> required "TaskId" (maybe Decode.int)
        |> required "TaskTitle" (maybe Decode.string)
        |> required "Recording" (maybe Decode.string)
        |> required "RecordingDate" Decode.string
        |> required "RecordingDuration" Decode.int
        |> required "Enrollment" Decode.bool
        |> required "StaffId" Decode.int
        |> required "StaffName" (maybe Decode.string)
        |> required "HasVerbalConsent" Decode.bool
        |> hardcoded False


rows : RecordType -> Int -> Http.Request (List RecordRow)
rows recordType patientId =
    let
        recordTypeId =
            getId recordType

        url =
            "/People/PatientRecordsGrid?patientId=" ++ toString patientId ++ "&recordTypeId=" ++ toString recordTypeId
    in
        Decode.field "list" (Decode.list decodeRecordRow)
            |> Http.get url


deleteRequest : Int -> Cmd Msg
deleteRequest rowId =
    Http.send DeleteCompleted <| Http.getString ("/People/DeleteRecord?recordId=" ++ toString rowId)



-- update helper functions


getTaskId : Model -> Maybe Int
getTaskId model =
    let
        records =
            model.records
                |> List.filter (\t -> t.id == model.dropDownState.rowId)
                |> List.head
                |> Maybe.map (\t -> t.taskId)
    in
        Maybe.withDefault Nothing records


getMenuMessage : List RecordRow -> RecordType -> Int -> String -> MenuMessage
getMenuMessage records recordType recordId messageType =
    let
        maybeVerbalConsent =
            records
                |> List.filter (\t -> t.id == recordId)
                |> List.head
                |> Maybe.map (\t -> not t.hasVerbalConsent)

        recordTypeId =
            Just <| getId recordType
    in
        MenuMessage messageType recordId recordTypeId maybeVerbalConsent


flipConsent : List RecordRow -> Int -> RecordType -> List RecordRow
flipConsent records recordId recordType =
    case recordType of
        CallRecordings ->
            records
                |> List.map
                    (\t ->
                        if t.id == recordId then
                            { t | hasVerbalConsent = not t.hasVerbalConsent }
                        else
                            t
                    )

        _ ->
            records


flipDropDownOpen : List RecordRow -> Int -> List RecordRow
flipDropDownOpen records recordId =
    records
        |> List.map
            (\t ->
                if t.id == recordId then
                    { t | dropDownOpen = not t.dropDownOpen }
                else
                    { t | dropDownOpen = False }
            )


getLoadedState : Model -> WebResponse -> Model
getLoadedState model t =
    { model
        | facilityId = t.facilityId
        , records = t.records
    }



-- Filtering update helpers


filterFields : Filters -> FilterState -> Filters
filterFields flds filterState =
    let
        fieldName =
            filterState.name

        fieldText =
            filterState.value

        t =
            String.toLower fieldText
    in
        if fieldName == "Date Collected" then
            { flds | date = t }
        else if fieldName == "Doctor of Visit" then
            { flds | provider = t }
        else if fieldName == "Specialty" then
            { flds | specialty = t }
        else if fieldName == "Comments" then
            { flds | comments = t }
        else if fieldName == "Date Accessioned" then
            { flds | dateAccessioned = t }
        else if fieldName == "Name of Lab" then
            { flds | title = t }
        else if fieldName == "Name of Study" then
            { flds | title = t }
        else if fieldName == "Provider" then
            { flds | provider = t }
        else if fieldName == "Date" then
            { flds | recordingDate = t }
        else if fieldName == "Recording" then
            { flds | recording = t }
        else if fieldName == "Task" then
            { flds | taskTitle = t }
        else if fieldName == "During Enrollment" then
            { flds | enrollment = t }
        else if fieldName == "Consent" then
            { flds | hasVerbalConsent = t }
        else if fieldName == "User" then
            { flds | staffName = t }
        else if fieldName == "File Name" then
            { flds | fileName = t }
        else if fieldName == "Report Date" then
            { flds | reportDate = t }
            -- hospitilizations
        else if fieldName == "Hospitalization ID" then
            { flds | hospitalizationId = t }
        else if fieldName == "Admin Date" then
            { flds | dateOfAdmission = t }
        else if fieldName == "Discharge Date" then
            { flds | dateOfDischarge = t }
        else if fieldName == "Service Type" then
            { flds | hospitalizationServiceType = t }
        else if fieldName == "Discharge Recommendations" then
            { flds | recommendations = t }
        else if fieldName == "Discharge Physician" then
            { flds | dischargePhysician = t }
        else
            flds


filteredRecords : List RecordRow -> Filters -> RecordType -> List RecordRow
filteredRecords records filterFields recordType =
    case recordType of
        Hospitalizations ->
            records
                |> List.filter (\t -> String.contains filterFields.date (defaultLowerDateTime t.date))
                |> List.filter (\t -> String.contains filterFields.hospitalizationId (defaultIntToString t.hospitalizationId))
                |> List.filter (\t -> String.contains filterFields.dateOfAdmission (defaultDateTime t.dateOfAdmission))
                |> List.filter (\t -> String.contains filterFields.dateOfDischarge (defaultDateTime t.dateOfDischarge))
                |> List.filter (\t -> String.contains filterFields.hospitalizationServiceType (defaultLower t.hospitalizationServiceType))
                |> List.filter (\t -> String.contains filterFields.recommendations (defaultLower t.recommendations))
                |> List.filter (\t -> String.contains filterFields.dischargePhysician (defaultLower t.dischargePhysician))
                |> List.filter (\t -> String.contains filterFields.comments (defaultLower t.comments))

        _ ->
            records
                |> List.filter (\t -> String.contains filterFields.date (defaultLowerDateTime t.date))
                |> List.filter (\t -> String.contains filterFields.provider (defaultLower t.provider))
                |> List.filter (\t -> String.contains filterFields.specialty (defaultLower t.specialty))
                |> List.filter (\t -> String.contains filterFields.comments (defaultLower t.comments))
                |> List.filter (\t -> String.contains filterFields.dateAccessioned (defaultLowerDateTime t.dateAccessed))
                |> List.filter (\t -> String.contains filterFields.provider (defaultLower t.provider))
                |> List.filter (\t -> String.contains filterFields.title (defaultLower t.title))
                |> List.filter (\t -> String.contains filterFields.recordingDate (toLower (dateTime t.recordingDate)))
                |> List.filter (\t -> String.contains filterFields.recording (defaultLower t.recording))
                |> List.filter (\t -> String.contains filterFields.taskTitle (defaultLower t.taskTitle))
                |> List.filter (\t -> String.contains filterFields.enrollment (toLower (toString t.enrollment)))
                |> List.filter (\t -> String.contains filterFields.hasVerbalConsent (toLower (toString t.hasVerbalConsent)))
                |> List.filter (\t -> String.contains filterFields.staffName (defaultLower t.staffName))
                |> List.filter (\t -> String.contains filterFields.fileName (defaultLower t.fileName))
                |> List.filter (\t -> String.contains filterFields.reportDate (defaultLowerDate t.reportDate))
