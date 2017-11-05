port module Records.Main exposing (..)

import Records.Load exposing (..)
import Records.Model exposing (..)
import Html exposing (Html, text, div, input, program, button, select, option, span, a, ul, li)
import Html.Attributes exposing (style, class, placeholder, id, type_, value, tabindex, tabindex)
import Html.Events exposing (onClick, onInput, on)
import Table
import Utils.CommonGrid exposing (..)


port viewFile : Int -> Cmd msg


init : Cmd Msg
init =
    getRecords Load


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditStart t ->
            { model | state = Edit t } ! []

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


view : Model -> Html Msg
view model =
    case model.state of
        Initial ->
            div [] [ text "loading" ]

        Grid ->
            div []
                [ div [ class "e-grid e-js e-waitingpopup" ]
                    [ Table.view config model.tableState model.records ]
                ]

        Edit rec ->
            div []
                [ input [ placeholder "Date of birth", type_ "text", class "e-textbox", id "testDate", value (defaultString rec.dateAccessed) ] []
                ]

        Error err ->
            div [] [ text (toString err) ]


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
        , viewData = editDropdownList x
        , sorter = Table.unsortable
        }


type alias DropDownMenuItem =
    { iconClass : String
    , displayText : String
    , event : Html.Attribute Msg
    }


x : List DropDownMenuItem
x =
    [ { iconClass = "", displayText = "", event = onClick Reset } ]


dropDownMenuItem : DropDownMenuItem -> Html Msg
dropDownMenuItem { iconClass, displayText, event } =
    li [ class "e-content e-list" ]
        [ a [ class "e-menulink", event ]
            [ text displayText
            , span [ class ("e-gridcontext e-icon " ++ iconClass) ] []
            ]
        ]


dropDownMenu : List DropDownMenuItem -> Record -> Html Msg
dropDownMenu dropDownMenuItems record =
    let
        dropDownMenuStyle =
            [ ( "margin-top", "-12px" )
            , ( "margin-right", "21px" )
            , ( "z-index", "5000" )
            , ( "position", "relative" )
            ]
    in
        div [ class "e-menu-wrap", style dropDownMenuStyle ]
            [ ul [ class "e-menu e-js e-widget e-box e-separator", tabindex 0 ]
                (List.map dropDownMenuItem dropDownMenuItems)
            ]


editDropdownList : List DropDownMenuItem -> Record -> Table.HtmlDetails Msg
editDropdownList dropDownItems record =
    let
        dropDownList =
            case record.dropDownState of
                DropdownClosed ->
                    div [] []

                DropdownOpen ->
                    (dropDownMenu dropDownItems record)
    in
        Table.HtmlDetails []
            [ div [ style [ ( "text-align", "right" ) ] ]
                [ button [ class "btn btn-sm btn-default fa fa-angle-down btn-context-menu", onClick (DropdownToggle record) ] []
                , dropDownList
                ]
            ]
