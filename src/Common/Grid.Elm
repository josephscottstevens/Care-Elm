module Common.Grid exposing (..)

import Html exposing (Html, text, div, button, input, span, th, li, ul, a)
import Html.Attributes exposing (style, class, type_, id, tabindex, attribute, href, target, checked, hidden, id, disabled)
import Html.Events as Events
import Table
import Common.Types exposing (FilterState)
import Json.Decode as Json


onInput : String -> (FilterState -> msg) -> Html.Attribute msg
onInput name handler =
    Events.on "input" <|
        Json.map handler <|
            Json.map2 FilterState (Json.succeed name) (Json.at [ "target", "value" ] Json.string)


onInputClear : String -> (FilterState -> msg) -> Html.Attribute msg
onInputClear name handler =
    Events.on "click" <|
        Json.map handler <|
            Json.map2 FilterState (Json.succeed name) (Json.succeed "")


dataTarget : String -> Html.Attribute msg
dataTarget val =
    attribute "data-target" val


standardTheadHelp : (FilterState -> msg) -> ( String, Table.Status, Html.Attribute msg ) -> Html msg
standardTheadHelp event ( name, status, onClick ) =
    let
        headerDivClass =
            class "e-headercelldiv e-gridtooltip headerColumn"

        headerDivStyle =
            style [ ( "margin-top", "5px" ) ]

        headerDiv =
            div [ headerDivClass, headerDivStyle, onClick ]

        headerColumnClass =
            class ("e-columnheader e-default e-filterbarcell " ++ name ++ "-Column")

        headerContent =
            case status of
                Table.Unsortable ->
                    headerDiv [ text name ]

                Table.Sortable selected ->
                    headerDiv [ text name ]

                Table.Reversible Nothing ->
                    headerDiv [ text name ]

                Table.Reversible (Just isReversed) ->
                    if isReversed then
                        headerDiv [ text name, span [ class "glyphicon glyphicon-menu-up" ] [] ]
                    else
                        headerDiv [ text name, span [ class "glyphicon glyphicon-menu-down" ] [] ]

        filterStyle =
            style [ ( "margin-bottom", "5px" ), ( "margin-top", "15px" ), ( "padding-left", "5px" ) ]

        filterCancelStyle =
            style [ ( "top", "22px" ), ( "right", "7%" ), ( "display", "none" ) ]

        filterContent =
            if name == "" then
                div [] []
            else
                div [ class "e-filterdiv e-fltrinputdiv" ]
                    [ input [ class "e-ejinputtext e-filtertext", filterStyle, onInput name event, id ("filter-" ++ name) ] []
                    , span [ class ("e-cancel e-icon "), id ("filter-" ++ name ++ "-btn"), filterCancelStyle, onInputClear name event ] []
                    ]
    in
        th [ headerColumnClass ] [ headerContent, filterContent ]


standardThead : (FilterState -> msg) -> List ( String, Table.Status, Html.Attribute msg ) -> Table.HtmlDetails msg
standardThead event headers =
    Table.HtmlDetails [ class "e-gridheader e-columnheader e-hidelines" ] (List.map (standardTheadHelp event) headers)


standardTableAttrs : String -> List (Html.Attribute msg)
standardTableAttrs idValue =
    [ id idValue, class "", style [ ( "width", "100%" ) ] ]


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


checkColumn : String -> (data -> Bool) -> Table.Column data msg
checkColumn name toCheck =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> checkColumnCell (toCheck data)
        , sorter = Table.unsortable
        }


checkColumnCell : Bool -> Table.HtmlDetails msg
checkColumnCell isChecked =
    Table.HtmlDetails []
        [ div [ class "e-checkcell" ]
            [ div [ class "e-checkcelldiv", style [ ( "text-align", "center" ) ] ]
                [ input [ type_ "checkbox", disabled True, checked isChecked ] []
                ]
            ]
        ]


hrefColumn : String -> String -> (data -> String) -> Table.Column data msg
hrefColumn name hrefText toHref =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> hrefColumnCell hrefText (toHref data)
        , sorter = Table.unsortable
        }


hrefColumnCell : String -> String -> Table.HtmlDetails msg
hrefColumnCell displayText url =
    Table.HtmlDetails []
        [ a [ href url, target "_blank" ] [ text displayText ]
        ]


hrefColumnExtra : String -> (data -> String) -> String -> msg -> Table.Column data msg
hrefColumnExtra name hrefText toHref event =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> hrefColumnCellExtra (hrefText data) toHref event
        , sorter = Table.unsortable
        }


hrefColumnCellExtra : String -> String -> msg -> Table.HtmlDetails msg
hrefColumnCellExtra displayText url event =
    Table.HtmlDetails []
        [ a [ href url, Events.onClick event ] [ text displayText ]
        ]


rowDropDownDiv : Bool -> Html.Attribute msg -> List ( String, String, Html.Attribute msg ) -> Table.HtmlDetails msg
rowDropDownDiv isVisible event dropDownItems =
    let
        dropMenu =
            case isVisible of
                True ->
                    [ ul [ class "e-menu e-js e-widget e-box e-separator", tabindex 0 ]
                        (List.map dropDownMenuItem dropDownItems)
                    ]

                False ->
                    []
    in
        Table.HtmlDetails []
            [ div [ style [ ( "text-align", "right" ) ] ]
                [ button [ type_ "button", class "btn btn-sm btn-default fa fa-angle-down btn-context-menu editDropDown", event, style [ ( "position", "relative" ) ] ]
                    [ div [ id "editButtonMenu", dropDownMenuStyle ]
                        dropMenu
                    ]
                ]
            ]


dropDownMenuStyle : Html.Attribute msg
dropDownMenuStyle =
    style
        [ ( "z-index", "5000" )
        , ( "position", "absolute" )
        , ( "display", "block" )
        , ( "left", "-173px" )
        , ( "width", "178.74px" )
        ]


dropDownMenuItem : ( String, String, Html.Attribute msg ) -> Html msg
dropDownMenuItem ( iconClass, displayText, event ) =
    li [ class "e-content e-list" ]
        [ a [ class "e-menulink", event, target "_blank" ]
            [ text displayText
            , span [ class ("e-gridcontext e-icon " ++ iconClass) ] []
            ]
        ]
