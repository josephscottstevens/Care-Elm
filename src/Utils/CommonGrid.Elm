module Utils.CommonGrid exposing (..)

import Html exposing (Html, text, div, program, button, input, span, th, li, ul, a, label)
import Html.Attributes exposing (style, class, type_, id, value, tabindex, for, attribute, href, target)
import Table
import Utils.CommonTypes exposing (DropDownState)
import Date
import Date.Extra


dataTarget : String -> Html.Attribute msg
dataTarget val =
    attribute "data-target" val


defaultString : Maybe String -> String
defaultString str =
    Maybe.withDefault "" str


defaultInt : Maybe Int -> String
defaultInt int =
    case int of
        Just t ->
            toString t

        Nothing ->
            ""


dateTimeToString : Date.Date -> String
dateTimeToString date =
    Date.Extra.toFormattedString "MM/dd/yyyy hh:mm:ss a" date


dateToString : Date.Date -> String
dateToString date =
    Date.Extra.toFormattedString "MM/dd/yyyy" date


defaultDateTime : Maybe String -> String
defaultDateTime maybeStr =
    case maybeStr of
        Just str ->
            case Date.fromString (str) of
                Ok date ->
                    dateTimeToString date

                Err _ ->
                    ""

        Nothing ->
            ""


defaultDate : Maybe String -> String
defaultDate maybeStr =
    case maybeStr of
        Just str ->
            case Date.fromString (str) of
                Ok date ->
                    dateToString date

                Err _ ->
                    ""

        Nothing ->
            ""


simpleTheadHelp : ( String, Table.Status, Html.Attribute msg ) -> Html msg
simpleTheadHelp ( name, status, onClick ) =
    let
        headerDivClass =
            class "e-headercelldiv e-gridtooltip headerColumn"

        headerDivStyle =
            style [ ( "margin-top", "5px" ) ]

        headerDiv =
            div [ headerDivClass, headerDivStyle ] [ text name ]

        headerColumnClass =
            class ("e-columnheader e-default e-filterbarcell " ++ name ++ "-Column")

        headerContent =
            case status of
                Table.Unsortable ->
                    headerDiv

                Table.Sortable selected ->
                    headerDiv

                Table.Reversible Nothing ->
                    headerDiv

                Table.Reversible (Just isReversed) ->
                    if isReversed then
                        div [ class "glyphicon glyphicon-menu-up" ] [ headerDiv ]
                    else
                        div [ class "glyphicon glyphicon-menu-down" ] [ headerDiv ]

        filterContent =
            if name == "" then
                div [] []
            else
                div [ class "e-filterdiv e-fltrinputdiv" ]
                    [ input [ class "e-ejinputtext e-filtertext", style [ ( "margin-bottom", "5px" ), ( "margin-top", "15px" ) ] ] []
                    ]

        content =
            [ headerContent, filterContent ]
    in
        th [ onClick, headerColumnClass ] content


simpleThead : List ( String, Table.Status, Html.Attribute msg ) -> Table.HtmlDetails msg
simpleThead headers =
    Table.HtmlDetails [ class "e-gridheader e-textover e-hidelines" ] (List.map simpleTheadHelp headers)


checkColumn : String -> Table.Column data msg
checkColumn name =
    Table.veryCustomColumn
        { name = name
        , viewData = checkColumnCell
        , sorter = Table.unsortable
        }


checkColumnCell : data -> Table.HtmlDetails msg
checkColumnCell emp =
    Table.HtmlDetails []
        [ div [ class "e-checkcell" ]
            [ div [ class "e-checkcelldiv", style [ ( "text-align", "center" ) ] ]
                [ input [ type_ "checkbox" ] []
                ]
            ]
        ]


editColumn : (data -> Html.Attribute msg) -> Table.Column data msg
editColumn toMsg =
    Table.veryCustomColumn
        { name = ""
        , viewData = editColumnCell << toMsg
        , sorter = Table.unsortable
        }


editColumnCell : Html.Attribute msg -> Table.HtmlDetails msg
editColumnCell clickEvent =
    Table.HtmlDetails []
        [ div [ style [ ( "text-align", "right" ) ] ]
            [ button [ class "btn btn-sm btn-default fa fa-angle-down btn-context-menu", clickEvent ] []
            ]
        ]


simpleRowAttrs : (a -> Int) -> a -> List (Html.Attribute msg)
simpleRowAttrs toInt =
    \data -> viewData (toInt data)


viewData : Int -> List (Html.Attribute msg)
viewData int =
    if int % 2 == 0 then
        [ class "e-row" ]
    else
        [ class "e-alt_row" ]


editButtonDiv : data -> Table.HtmlDetails msg
editButtonDiv data =
    Table.HtmlDetails []
        [ div [ style [ ( "text-align", "right" ) ] ]
            [ button [ type_ "button", class "btn btn-sm btn-default fa fa-angle-down btn-context-menu editDropDown", dataTarget (toString data) ] []
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
    style
        [ ( "left", toString dropDownState.x ++ "px" )
        , ( "top", toString dropDownState.y ++ "px" )
        , ( "z-index", "5000" )
        , ( "position", "absolute" )
        , ( "display", "block" )
        ]


dropDownMenuItem : ( String, String, Html.Attribute msg ) -> Html msg
dropDownMenuItem ( iconClass, displayText, event ) =
    li [ class "e-content e-list" ]
        [ a [ class "e-menulink", event, target "_blank" ]
            [ text displayText
            , span [ class ("e-gridcontext e-icon " ++ iconClass) ] []
            ]
        ]
