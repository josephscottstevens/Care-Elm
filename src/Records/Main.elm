port module Records.Main exposing (..)

import Records.Load exposing (..)
import Records.Model exposing (..)
import Html exposing (Html, text, div, input, program, button, select, option, span, a, ul, li, label, form, textarea, img)
import Html.Attributes exposing (style, class, id, type_, value, tabindex, tabindex, for, src, title)
import Html.Events exposing (onInput, onClick, onInput, onSubmit)
import Table
import Utils.CommonGrid exposing (..)
import Utils.CommonHtml exposing (..)
import Utils.CommonTypes exposing (..)


port sendMenuMessage : MenuMessage -> Cmd msg


port initSyncfusionControls : SyncFusionMessage -> Cmd msg


port deleteComplete : String -> Cmd msg


port submitForm : NewRecord -> Cmd msg


port setLoadingStatus : Bool -> Cmd msg


port setUnsavedChanges : Bool -> Cmd msg


port saveComplete : (String -> msg) -> Sub msg


port updateFacility : (DropDownItem -> msg) -> Sub msg


port updateCategory : (DropDownItem -> msg) -> Sub msg


port updateDateTimeOfVisit : (String -> msg) -> Sub msg


port dropDownToggle : (DropDownState -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        AddNew t ->
            Sub.batch
                [ updateFacility (UpdateFacility t)
                , updateCategory (UpdateCategory t)
                , updateDateTimeOfVisit (UpdateDateTimeOfVisit t)
                , saveComplete SaveCompleted
                ]

        _ ->
            Sub.batch
                [ dropDownToggle DropDownToggle
                ]


init : Flags -> Cmd Msg
init flag =
    case flag.recordType of
        Just recType ->
            (getRecords flag.patientId recType) Load

        Nothing ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load (Ok t) ->
            { model
                | state = Grid
                , records = t.records
                , facilities = t.facilities
            }
                ! [ setLoadingStatus False ]

        Load (Err t) ->
            { model | state = Error t } ! [ setLoadingStatus False ]

        SetTableState newState ->
            { model | tableState = newState } ! []

        SendMenuMessage recordId messageType ->
            model ! [ sendMenuMessage (MenuMessage messageType recordId model.recordTypeId) ]

        Delete rowId ->
            let
                updatedRecords =
                    model.records |> List.filter (\t -> t.id /= rowId)
            in
                ( { model | records = updatedRecords }, deleteRequest rowId )

        AddNewStart ->
            let
                newRecord =
                    { emptyNewRecord | patientId = model.patientId, recordTypeId = model.recordTypeId }
            in
                ( { model | state = AddNew newRecord }, initSyncfusionControls (SyncFusionMessage model.facilities model.recordTypeId) )

        Save newRecord ->
            let
                action =
                    if List.length (formValidationErrors newRecord) > 0 then
                        Cmd.none
                    else
                        submitForm newRecord
            in
                { model | state = AddNew { newRecord | showValidationErrors = True } } ! [ action, setUnsavedChanges False ]

        SaveCompleted str ->
            ( model, (getRecords model.patientId model.recordTypeId) Load )

        Cancel ->
            { model | state = Grid } ! [ setUnsavedChanges False ]

        DropDownToggle dropState ->
            { model | dropDownState = dropState } ! []

        DeleteCompleted (Ok t) ->
            ( model, deleteComplete "Record was deleted successfully" )

        DeleteCompleted (Err t) ->
            { model | state = Error t } ! []

        UpdateFacility newRecord dropDownItem ->
            { model | state = AddNew { newRecord | facility = dropDownItem.name, facilityId = dropDownItem.id } } ! [ setUnsavedChanges True ]

        UpdateCategory newRecord dropDownItem ->
            case dropDownItem.id of
                Just t ->
                    { model | state = AddNew { newRecord | recordType = dropDownItem.name, recordTypeId = t } } ! [ setUnsavedChanges True ]

                Nothing ->
                    model ! []

        UpdateDateTimeOfVisit newRecord str ->
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

                submitBtnType =
                    if List.length errors > 0 then
                        "button"
                    else
                        "submit"

                validationErrorsDiv =
                    if newRecord.showValidationErrors == True then
                        displayErrors errors
                    else
                        div [] []
            in
                div
                    [ class "form-horizontal" ]
                    [ validationErrorsDiv
                    , dropInput "Facility"
                    , dropInput "Category"
                    , textInput input "Date of Visit" newRecord.timeVisit (UpdateDateTimeOfVisit newRecord) True
                    , textInput input "Doctor of Visit" newRecord.provider (UpdateDoctorOfVisit newRecord) False
                    , textInput input "Speciality of Visit" newRecord.speciality (UpdateSpecialtyOfVisit newRecord) False
                    , textInput input "Comments" newRecord.comments (UpdateComments newRecord) True
                    , fileInput input "Upload Record File" "" (UpdateRecordFile newRecord) True
                    , hideInput "FacilityID" "79"
                    , hideInput "PatientID" "6676"
                    , hideInput "Recordtype" "1"
                    , div [ class "form-group" ]
                        [ div [ class fullWidth ]
                            [ button [ type_ "button", id "Save", value "AddNewRecord", onClick (Save newRecord), class "btn btn-success margin-left-5 pull-right" ] [ text "Save" ]
                            , button [ type_ "button", onClick Cancel, class "btn btn-default pull-right" ] [ text "Cancel" ]
                            ]
                        ]
                    ]

        Error err ->
            div [] [ text (toString err) ]


formValidationErrors : NewRecord -> List String
formValidationErrors newRecord =
    let
        errors =
            [ required newRecord.recordType "Category"
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


getColumns : Int -> List (Table.Column Record Msg)
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
        firstColumn :: (List.append middleColumns lastColumns)


config : Int -> Table.Config Record Msg
config recordTypeId =
    Table.customConfig
        { toId = (\t -> toString .id)
        , toMsg = SetTableState
        , columns = getColumns recordTypeId
        , customizations = defaultCustomizations
        }


defaultCustomizations : Table.Customizations Record msg
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


editButton : Table.Column Record msg
editButton =
    Table.veryCustomColumn
        { name = ""
        , viewData = editButtonDiv << .id
        , sorter = Table.unsortable
        }
