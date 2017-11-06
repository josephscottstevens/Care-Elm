port module Records.Main exposing (..)

import Records.Load exposing (..)
import Records.Model exposing (..)
import Html exposing (Html, text, div, input, program, button, select, option, span, a, ul, li, label, form, textarea)
import Html.Attributes exposing (style, class, id, type_, value, tabindex, tabindex, for)
import Html.Events exposing (onInput, onClick, onInput)
import Table
import Utils.CommonGrid exposing (..)


port viewFile : Int -> Cmd msg


port initSyncfusionControls : String -> Cmd msg


port updateFacility : (String -> msg) -> Sub msg


port updateCategory : (String -> msg) -> Sub msg


port updateDateTimeOfVisit : (String -> msg) -> Sub msg


port updateRecordFile : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ updateFacility UpdateFacility
        , updateCategory UpdateCategory
        , updateDateTimeOfVisit UpdateDateTimeOfVisit
        ]


init : Cmd Msg
init =
    getRecords Load


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddNewStart ->
            ( { model | state = AddNew }, initSyncfusionControls "" )

        Load (Ok t) ->
            { t | state = Grid } ! []

        Load (Err t) ->
            { model | state = Error t } ! []

        SetTableState newState ->
            { model | tableState = newState } ! []

        Reset ->
            emptyModel ! []

        ViewFile recordId ->
            ( model, viewFile recordId )

        Delete record ->
            ( model, deleteRequest record )

        DropdownToggle record ->
            let
                newRecord =
                    { record | dropDownOpen = not record.dropDownOpen }
            in
                { model | records = updateRecords model.records newRecord } ! []

        DeleteCompleted (Ok t) ->
            ( model, init )

        DeleteCompleted (Err t) ->
            { model | state = Error t } ! []

        UpdateFacility str ->
            { model | addNewRecord = (setFacility str model.addNewRecord) } ! []

        UpdateCategory str ->
            { model | addNewRecord = (setCategory str model.addNewRecord) } ! []

        UpdateDateTimeOfVisit str ->
            { model | addNewRecord = (setDateTimeOfVisit str model.addNewRecord) } ! []

        UpdateDoctorOfVisit str ->
            { model | addNewRecord = (setDoctorOfVisit str model.addNewRecord) } ! []

        UpdateSpecialtyOfVisit str ->
            { model | addNewRecord = (setSpecialtyOfVisit str model.addNewRecord) } ! []

        UpdateComments str ->
            { model | addNewRecord = (setComments str model.addNewRecord) } ! []

        UpdateRecordFile str ->
            { model | addNewRecord = (setRecordFile str model.addNewRecord) } ! []

        Cancel ->
            { model | state = Grid } ! []


view : Model -> Html Msg
view model =
    case model.state of
        Initial ->
            div [] [ text "loading" ]

        Grid ->
            div []
                [ button [ class "btn btn-default", onClick AddNewStart ] [ text "New Record" ]
                , div [ class "e-grid e-js e-waitingpopup" ]
                    [ Table.view config model.tableState model.records ]
                ]

        AddNew ->
            form [ class "form-horizontal" ]
                [ eInput "Facility" "FacilityId" model.addNewRecord.facility
                , eInput "Category" "CategoryId" model.addNewRecord.category
                , eInput "Date of Visit" "DateTimeOfVisitId" model.addNewRecord.dateTimeOfVisit
                , eTextBox "Doctor of Visit" "DoctorOfVisitId" model.addNewRecord.doctorOfVisit UpdateDoctorOfVisit
                , eTextBox "Speciality of Visit" "SpecialityOfVisitId" model.addNewRecord.specialityOfVisit UpdateFacility
                , eTextArea "Comments" "CommentsId" model.addNewRecord.comments UpdateFacility
                , eFileUpload "Upload Record File" "RecordFileId" model.addNewRecord.recordFile
                , div [ class "form-group" ]
                    [ div [ class "col-sm-10 col-md-7 col-lg-6" ]
                        [ button [ type_ "button", onClick Cancel, class "btn btn-primary margin-left-5 pull-right" ] [ text "Save" ]
                        , button [ type_ "button", onClick Cancel, class "btn btn-default pull-right" ] [ text "Cancel" ]
                        ]
                    ]
                ]

        Error err ->
            div [] [ text (toString err) ]


eInput : String -> String -> String -> Html msg
eInput displayText idAttr inputValue =
    div [ class "form-group" ]
        [ label [ class "col-sm-2 col-md-2 col-lg-2 control-label required", for idAttr ] [ text displayText ]
        , div [ class "col-sm-8 col-md-5 col-lg-4" ]
            [ input [ type_ "text", class "e-textbox", id idAttr, value inputValue ] [] ]
        ]


eTextArea : String -> String -> String -> (String -> msg) -> Html msg
eTextArea displayText idAttr inputValue event =
    div [ class "form-group" ]
        [ label [ class "col-sm-2 col-md-2 col-lg-2 control-label required", for idAttr ] [ text displayText ]
        , div [ class "col-sm-8 col-md-5 col-lg-4" ]
            [ textarea [ class "e-textbox", id idAttr, value inputValue, onInput event ] [] ]
        ]


eTextBox : String -> String -> String -> (String -> msg) -> Html msg
eTextBox displayText idAttr inputValue event =
    div [ class "form-group" ]
        [ label [ class "col-sm-2 col-md-2 col-lg-2 control-label required", for idAttr ] [ text displayText ]
        , div [ class "col-sm-8 col-md-5 col-lg-4" ]
            [ input [ type_ "text", class "e-textbox", id idAttr, value inputValue, onInput event ] [] ]
        ]


eFileUpload : String -> String -> String -> Html msg
eFileUpload displayText idAttr inputValue =
    div [ class "form-group" ]
        [ label [ class "col-sm-2 col-md-2 col-lg-2 control-label required", for idAttr ] [ text displayText ]
        , div [ class "col-sm-8 col-md-5 col-lg-4" ]
            [ div [ class "e-textbox", id idAttr, value inputValue ] [] ]
        ]


config : Table.Config Record Msg
config =
    Table.customConfig
        { toId = (\t -> toString .id)
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Date Collected" (\t -> defaultString t.date)
            , Table.stringColumn "Doctor of Visit" (\t -> defaultString t.provider)
            , Table.stringColumn "Speciality" (\t -> defaultString t.speciality)
            , Table.stringColumn "Comments" (\t -> defaultString t.comments)
            , editDropdown
            ]
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


editDropdown : Table.Column Record Msg
editDropdown =
    Table.veryCustomColumn
        { name = ""
        , viewData = editDropdownList
        , sorter = Table.unsortable
        }


dropDownItems : Record -> List ( String, String, Html.Attribute Msg )
dropDownItems record =
    [ ( "e-contextedit", "View File", onClick (ViewFile record.id) )
    , ( "e-contextdelete", "Delete", onClick (Delete record) )
    ]


editDropdownList : Record -> Table.HtmlDetails Msg
editDropdownList record =
    buildDropDown (dropDownItems record) record.dropDownOpen (onClick (DropdownToggle record))
