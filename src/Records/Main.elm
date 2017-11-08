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


port viewFile : Int -> Cmd msg


port initSyncfusionControls : SyncFusionMessage -> Cmd msg


port deleteComplete : String -> Cmd msg


port submitForm : NewRecord -> Cmd msg


port saveComplete : (String -> msg) -> Sub msg


port updateFacility : (String -> msg) -> Sub msg


port updateCategory : (String -> msg) -> Sub msg


port updateDateTimeOfVisit : (String -> msg) -> Sub msg


port dropDownToggle : (DropDownState -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ updateFacility UpdateFacility
        , updateCategory UpdateCategory
        , updateDateTimeOfVisit UpdateDateTimeOfVisit
        , saveComplete SaveCompleted
        , dropDownToggle DropDownToggle
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
        AddNewStart ->
            ( { model | state = AddNew }, initSyncfusionControls (SyncFusionMessage model.facilities model.recordTypeId) )

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

        Delete rowId ->
            ( model, deleteRequest rowId )

        Save newRecord ->
            let
                action =
                    if List.length (formValidationErrors model.addNewRecord) > 0 then
                        Cmd.none
                    else
                        submitForm newRecord
            in
                ( { model | showValidationErrors = True }, action )

        SaveCompleted str ->
            ( emptyModel, (getRecords model.patientId model.recordTypeId) Load )

        DropDownToggle dropState ->
            { model | dropDownState = dropState } ! []

        DeleteCompleted (Ok t) ->
            ( emptyModel, Cmd.batch [ (getRecords model.patientId model.recordTypeId) Load, deleteComplete "Record was deleted successfully" ] )

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
                [ button [ type_ "button", class "btn btn-default margin-bottom-5", onClick AddNewStart ] [ text "New Record" ]
                , editDropDownDiv (dropDownItems model.dropDownState.rowId) model.dropDownState
                , div [ class "e-grid e-js e-waitingpopup" ]
                    [ Table.view config model.tableState model.records ]
                ]

        AddNew ->
            let
                errors =
                    formValidationErrors model.addNewRecord

                submitBtnType =
                    if List.length errors > 0 then
                        "button"
                    else
                        "submit"

                validationErrorsDiv =
                    if model.showValidationErrors == True then
                        displayErrors errors
                    else
                        div [] []
            in
                div
                    [ class "form-horizontal" ]
                    [ validationErrorsDiv
                    , textInput input "Facility" model.addNewRecord.facility UpdateFacility False
                    , textInput input "Category" model.addNewRecord.category UpdateCategory True
                    , textInput input "Date of Visit" model.addNewRecord.dateTimeOfVisit UpdateDateTimeOfVisit True
                    , textInput input "Doctor of Visit" model.addNewRecord.doctorOfVisit UpdateDoctorOfVisit False
                    , textInput input "Speciality of Visit" model.addNewRecord.specialityOfVisit UpdateSpecialtyOfVisit False
                    , textInput input "Comments" model.addNewRecord.comments UpdateComments True
                    , fileInput input "Upload Record File" model.addNewRecord.recordFile UpdateRecordFile True
                    , hideInput "FacilityID" "79"
                    , hideInput "PatientID" "6676"
                    , hideInput "Recordtype" "1"
                    , div [ class "form-group" ]
                        [ div [ class fullWidth ]
                            [ button [ type_ "button", id "Save", value "AddNewRecord", onClick (Save model.addNewRecord), class "btn btn-success margin-left-5 pull-right" ] [ text "Save" ]
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
            [ required newRecord.category "Category"
            , required newRecord.dateTimeOfVisit "Date of Visit"
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
            , editButton
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



--public


dropDownItems : Int -> List ( String, String, Html.Attribute Msg )
dropDownItems rowId =
    [ ( "e-contextedit", "View File", onClick (ViewFile rowId) )
    , ( "e-contextdelete", "Delete", onClick (Delete rowId) )
    ]



-- private


editButton : Table.Column Record Msg
editButton =
    Table.veryCustomColumn
        { name = ""
        , viewData = editButtonDiv
        , sorter = Table.unsortable
        }


editButtonDiv : Record -> Table.HtmlDetails Msg
editButtonDiv record =
    Table.HtmlDetails []
        [ div [ style [ ( "text-align", "right" ) ] ]
            [ button [ type_ "button", class "btn btn-sm btn-default fa fa-angle-down btn-context-menu editDropDown", dataTarget (toString record.id) ] []
            ]
        ]


editDropDownDiv : List ( String, String, Html.Attribute msg ) -> DropDownState -> Html msg
editDropDownDiv dropDownItems dropDownState =
    div [ id "editButtonMenu", dropDownMenuStyle dropDownState ]
        [ ul [ class "e-menu e-js e-widget e-box e-separator", tabindex 0 ]
            (List.map dropDownMenuItem dropDownItems)
        ]


dropDownMenuStyle : DropDownState -> Html.Attribute msg
dropDownMenuStyle dropDownState =
    let
        show =
            if dropDownState.showEditMenu == True then
                ("")
            else
                ("none")
    in
        style
            [ ( "left", toString dropDownState.x ++ "px" )
            , ( "top", toString dropDownState.y ++ "px" )
            , ( "z-index", "5000" )
            , ( "position", "absolute" )
            , ( "display", show )
            ]


dropDownMenuItem : ( String, String, Html.Attribute msg ) -> Html msg
dropDownMenuItem ( iconClass, displayText, event ) =
    li [ class "e-content e-list" ]
        [ a [ class "e-menulink", event ]
            [ text displayText
            , span [ class ("e-gridcontext e-icon " ++ iconClass) ] []
            ]
        ]
