port module Records.Main exposing (..)

import Records.Load exposing (..)
import Records.Model exposing (..)
import Html exposing (Html, text, div, input, program, button, select, option, span, a, ul, li, label, form)
import Html.Attributes exposing (style, class, id, type_, value, tabindex, tabindex, for)
import Html.Events exposing (onClick, onInput, on)
import Table
import Utils.CommonGrid exposing (..)


port viewFile : Int -> Cmd msg


port sendDateTimeOfVisitId : String -> Cmd msg


port getDateTimeOfVisitId : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    getDateTimeOfVisitId UpdateStartDate


init : Cmd Msg
init =
    getRecords Load


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddNewStart ->
            ( { model | state = AddNew }, sendDateTimeOfVisitId "" )

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
                    case record.dropDownState of
                        DropdownOpen ->
                            { record | dropDownState = DropdownClosed }

                        DropdownClosed ->
                            { record | dropDownState = DropdownOpen }
            in
                { model | records = updateRecords model.records newRecord } ! []

        DeleteCompleted (Ok t) ->
            ( model, init )

        DeleteCompleted (Err t) ->
            { model | state = Error t } ! []

        UpdateStartDate str ->
            let
                t =
                    model.addNewRecord

                x =
                    { t | dateTimeOfVisit = str }
            in
                ( { model | addNewRecord = x }, Cmd.none )

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
                , div [ class "form-group" ]
                    [ div [ class "col-sm-offset-5 col-sm-4" ]
                        [ button [ onClick Cancel, class "btn btn-primary" ] [ text "Save" ]
                        , button [ onClick Cancel, class "btn btn-default margin-left-5" ] [ text "Cancel" ]
                        ]
                    ]
                ]

        Error err ->
            div [] [ text (toString err) ]


eInput : String -> String -> String -> Html msg
eInput displayText idAttr inputValue =
    div [ class "form-group" ]
        [ label [ class "col-sm-2 control-label required", for idAttr ] [ text displayText ]
        , div [ class "col-xs-10 col-sm-10 col-md-5 col-lg-4" ]
            [ input [ type_ "text", class "e-textbox", id idAttr, value inputValue ] [] ]
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
    buildDropDown (dropDownItems record) record.dropDownState (onClick (DropdownToggle record))
