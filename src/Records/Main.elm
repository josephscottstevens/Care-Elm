port module Records.Main exposing (..)

import Records.Load exposing (..)
import Records.Model exposing (..)
import Html exposing (Html, text, div, input, program, button, select, option, span, a, ul, li, label, form, textarea, img)
import Html.Attributes exposing (style, class, id, type_, value, tabindex, tabindex, for, src, title)
import Html.Events exposing (onInput, onClick, onInput, onSubmit)
import Table
import Utils.CommonGrid exposing (..)
import Utils.CommonHtml exposing (..)


port viewFile : Int -> Cmd msg


port initSyncfusionControls : String -> Cmd msg


port updateFacility : (String -> msg) -> Sub msg


port updateCategory : (String -> msg) -> Sub msg


port updateDateTimeOfVisit : (String -> msg) -> Sub msg


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

        Save newRecord ->
            ( { model | showValidationErrors = True }, Cmd.none )

        DropDownToggle record ->
            let
                newRecord =
                    { record | dropDownOpen = not record.dropDownOpen }
            in
                { model | records = updateRecords model.records newRecord } ! []

        DeleteCompleted (Ok t) ->
            ( model, Cmd.none )

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
                [ button [ type_ "button", class "btn btn-default", onClick AddNewStart ] [ text "New Record" ]
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

                submitBtnEvent =
                    if List.length errors > 0 then
                        onClick
                    else
                        onSubmit

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
                    , div [ class "form-group" ]
                        [ div [ class fullWidth ]
                            [ button [ type_ "button", value "AddNewRecord", submitBtnEvent (Save model.addNewRecord), class "btn btn-primary margin-left-5 pull-right" ] [ text "Save" ]
                            , button [ type_ submitBtnType, onClick Cancel, class "btn btn-default pull-right" ] [ text "Cancel" ]
                            ]
                        ]
                    ]

        Error err ->
            div [] [ text (toString err) ]


formValidationErrors : Records.Model.NewRecord -> List String
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
            , editDropDown
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


editDropDown : Table.Column Record Msg
editDropDown =
    Table.veryCustomColumn
        { name = ""
        , viewData = editDropDownList
        , sorter = Table.unsortable
        }


dropDownItems : Record -> List ( String, String, Html.Attribute Msg )
dropDownItems record =
    [ ( "e-contextedit", "View File", onClick (ViewFile record.id) )
    , ( "e-contextdelete", "Delete", onClick (Delete record) )
    ]


editDropDownList : Record -> Table.HtmlDetails Msg
editDropDownList record =
    buildDropDown (dropDownItems record) record.dropDownOpen (onClick (DropDownToggle record))
