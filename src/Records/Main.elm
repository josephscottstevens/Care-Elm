port module Records.Main exposing (..)

import Records.Load exposing (..)
import Records.Model exposing (..)
import Html exposing (Html, text, div, button)
import Html.Attributes exposing (class, id, type_, value)
import Html.Events exposing (onClick)
import Table
import Utils.CommonGrid exposing (..)
import Utils.CommonHtml exposing (..)
import Utils.CommonTypes exposing (..)


port sendMenuMessage : MenuMessage -> Cmd msg


port initSyncfusionControls : SyncFusionMessage -> Cmd msg


port deleteComplete : String -> Cmd msg


port setLoadingStatus : Bool -> Cmd msg


port setUnsavedChanges : Bool -> Cmd msg


port updateFacility : (DropDownItem -> msg) -> Sub msg


port updateCategory : (DropDownItem -> msg) -> Sub msg


port updateDateTimeOfVisit : (String -> msg) -> Sub msg


port updateFileName : (String -> msg) -> Sub msg


port dropDownToggle : (DropDownState -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        AddNew t ->
            Sub.batch
                [ updateFacility (UpdateFacility t)
                , updateCategory (UpdateCategory t)
                , updateDateTimeOfVisit (UpdateDateOfVisit t)
                , updateFileName (UpdateRecordFile t)
                ]

        _ ->
            Sub.batch
                [ dropDownToggle DropDownToggle
                ]


init : Flags -> Cmd Msg
init flag =
    case flag.recordType of
        Just recType ->
            getRecords flag.patientId recType Load

        Nothing ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load (Ok t) ->
            { model
                | state = Grid
                , facilityId = t.facilityId
                , records = t.records
                , facilities = t.facilities
                , recordTypes = t.recordTypes
            }
                ! [ setLoadingStatus False ]

        Load (Err httpError) ->
            { model | state = Error (toString httpError) } ! [ setLoadingStatus False ]

        SetTableState newState ->
            { model | tableState = newState } ! []

        SendMenuMessage recordId messageType ->
            model ! [ sendMenuMessage (MenuMessage messageType recordId model.recordTypeId) ]

        Delete rowId ->
            let
                updatedRecords =
                    model.records |> List.filter (\t -> t.id /= rowId)
            in
                { model | records = updatedRecords } ! [ deleteRequest rowId ]

        AddNewStart ->
            let
                newRecord =
                    { emptyNewRecord
                        | patientId = model.patientId
                        , recordTypeId = model.recordTypeId
                        , facilityId = model.facilityId
                        , facilityText = getDropDownItemById model.facilities model.facilityId
                        , recordTypeText = getDropDownItemById model.recordTypes (Just model.recordTypeId)
                    }
            in
                { model | state = AddNew newRecord }
                    ! [ initSyncfusionControls (SyncFusionMessage model.facilities model.recordTypes model.facilityId model.recordTypeId) ]

        Save newRecord ->
            let
                actions =
                    if List.length (formValidationErrors newRecord) > 0 then
                        []
                    else
                        [ saveForm newRecord, setUnsavedChanges False ]
            in
                { model | state = AddNew { newRecord | showValidationErrors = True } } ! actions

        SaveCompleted (Ok _) ->
            model ! [ getRecords model.patientId model.recordTypeId Load ]

        SaveCompleted (Err httpError) ->
            { model | state = Error (toString httpError) } ! [ setLoadingStatus False ]

        Cancel ->
            { model | state = Grid } ! [ setUnsavedChanges False ]

        DropDownToggle dropState ->
            { model | dropDownState = dropState } ! []

        DeleteCompleted (Ok _) ->
            model ! [ deleteComplete "Record was deleted successfully" ]

        DeleteCompleted (Err httpError) ->
            { model | state = Error (toString httpError) } ! []

        UpdateFacility newRecord dropDownItem ->
            { model | state = AddNew { newRecord | facilityText = dropDownItem.name, facilityId = dropDownItem.id } } ! [ setUnsavedChanges True ]

        UpdateCategory newRecord dropDownItem ->
            case dropDownItem.id of
                Just t ->
                    { model | state = AddNew { newRecord | recordTypeText = dropDownItem.name, recordTypeId = t } } ! [ setUnsavedChanges True ]

                Nothing ->
                    model ! []

        UpdateDateOfVisit newRecord str ->
            { model | state = AddNew { newRecord | timeVisit = str } } ! [ setUnsavedChanges True ]

        UpdateDoctorOfVisit newRecord str ->
            { model | state = AddNew { newRecord | provider = str } } ! [ setUnsavedChanges True ]

        UpdateSpecialtyOfVisit newRecord str ->
            { model | state = AddNew { newRecord | speciality = str } } ! [ setUnsavedChanges True ]

        UpdateComments newRecord str ->
            { model | state = AddNew { newRecord | comments = str } } ! [ setUnsavedChanges True ]

        UpdateRecordFile newRecord str ->
            { model | state = AddNew { newRecord | recordFile = str } } ! [ setUnsavedChanges True ]


view : Model -> Html Msg
view model =
    case model.state of
        Grid ->
            div []
                [ button [ type_ "button", class "btn btn-default margin-bottom-5", onClick AddNewStart ] [ text "New Record" ]
                , editDropDownDiv (dropDownItems model.dropDownState.rowId) model.dropDownState
                , div [ class "e-grid e-js e-waitingpopup" ]
                    [ Table.view (config model.recordTypeId) model.tableState model.records ]
                ]

        AddNew newRecord ->
            let
                errors =
                    formValidationErrors newRecord

                validationErrorsDiv =
                    if newRecord.showValidationErrors == True && List.length errors > 0 then
                        displayErrors errors
                    else
                        div [] []

                saveBtnClass =
                    class "btn btn-success margin-left-5 pull-right"

                footerControls =
                    [ div [ class "form-group" ]
                        [ div [ class fullWidth ]
                            [ button [ type_ "button", id "Save", value "AddNewRecord", onClick (Save newRecord), saveBtnClass ] [ text "Save" ]
                            , button [ type_ "button", onClick Cancel, class "btn btn-default pull-right" ] [ text "Cancel" ]
                            ]
                        ]
                    ]

                inputControls =
                    makeControls (formInputs newRecord)
            in
                div
                    [ class "form-horizontal" ]
                    (validationErrorsDiv :: inputControls ++ footerControls)

        Error errMessage ->
            div [] [ text errMessage ]



-- Validation Stuff


formInputs : NewRecord -> List (InputControlType Msg)
formInputs newRecord =
    let
        recordType =
            getRecordType newRecord.recordTypeId

        defaultFields =
            [ DropInput Required "Date of Visit" "DateofVisitId"
            , TextInput Optional "Doctor of Visit" (UpdateDoctorOfVisit newRecord)
            , TextInput Optional "Speciality of Visit" (UpdateSpecialtyOfVisit newRecord)
            , AreaInput Required "Comments" (UpdateComments newRecord)
            , FileInput Required "Upload Record File" newRecord.recordFile
            ]

        firstColumns =
            [ DropInput Required "Facility" "FacilityId"
            , DropInput Required "Category" "CategoryId"
            ]

        lastColumns =
            case recordType of
                PrimaryCare ->
                    defaultFields

                Speciality ->
                    defaultFields

                Labs ->
                    []

                Radiology ->
                    []

                Misc ->
                    defaultFields

                Legal ->
                    []

                Hospitalizations ->
                    []

                CallRecordings ->
                    []

                PreviousHistories ->
                    []

                Enrollment ->
                    []
    in
        List.append firstColumns lastColumns


formValidationErrors : NewRecord -> List String
formValidationErrors newRecord =
    let
        errors =
            [ required newRecord.recordTypeText "Category"
            , required newRecord.timeVisit "Date of Visit"
            , required newRecord.comments "Comments"
            , required newRecord.recordFile "Record File"
            ]
    in
        errors |> List.filterMap identity


required : String -> String -> Maybe String
required str propName =
    if str == "" then
        Just (propName ++ " is required")
    else
        Nothing


displayErrors : List String -> Html Msg
displayErrors errors =
    div [ class "error" ] (List.map (\t -> div [] [ text t ]) errors)



-- Column Stuff


getColumns : Int -> List (Table.Column RecordRow Msg)
getColumns recordTypeId =
    let
        firstColumn =
            Table.stringColumn "Date Collected" (\t -> defaultString t.date)

        middleColumns =
            if recordTypeId == 1 || recordTypeId == 2 || recordTypeId == 5 then
                [ Table.stringColumn "Doctor of Visit" (\t -> defaultString t.provider)
                , Table.stringColumn "Speciality" (\t -> defaultString t.speciality)
                ]
            else if recordTypeId == 6 then
                []
            else
                []

        lastColumns =
            [ Table.stringColumn "Comments" (\t -> defaultString t.comments)
            , editButton
            ]
    in
        firstColumn :: List.append middleColumns lastColumns


config : Int -> Table.Config RecordRow Msg
config recordTypeId =
    Table.customConfig
        { toId = \t -> toString t.id
        , toMsg = SetTableState
        , columns = getColumns recordTypeId
        , customizations = defaultCustomizations
        }


defaultCustomizations : Table.Customizations RecordRow msg
defaultCustomizations =
    { tableAttrs = [ class "e-grid e-js e-waitingpopup" ]
    , caption = Nothing
    , thead = simpleThead
    , tfoot = Nothing
    , tbodyAttrs = []
    , rowAttrs = simpleRowAttrs .id
    }


dropDownItems : Int -> List ( String, String, Html.Attribute Msg )
dropDownItems rowId =
    [ ( "", "Transfer", onClick (SendMenuMessage rowId "Transfer") )
    , ( "e-contextedit", "View File", onClick (SendMenuMessage rowId "ViewFile") )
    , ( "", "Send By Email", onClick (SendMenuMessage rowId "SendByEmail") )
    , ( "", "Send By Fax", onClick (SendMenuMessage rowId "SendByFax") )
    , ( "", "Save To Client Portal", onClick (SendMenuMessage rowId "SaveToClientPortal") )
    , ( "e-contextdelete", "Delete", onClick (Delete rowId) )
    ]


editButton : Table.Column RecordRow msg
editButton =
    Table.veryCustomColumn
        { name = ""
        , viewData = editButtonDiv << .id
        , sorter = Table.unsortable
        }


getDropDownItemById : List DropDownItem -> Maybe Int -> String
getDropDownItemById facilities facilityId =
    case
        facilities
            |> List.filter (\t -> t.id == facilityId)
            |> List.head
    of
        Just dropDownItem ->
            dropDownItem.name

        Nothing ->
            ""
