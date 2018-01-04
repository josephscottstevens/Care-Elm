module Utils.CommonGrid exposing (..)

import Html exposing (Html, text, div, button, input, span, th, li, ul, a)
import Html.Attributes exposing (style, class, type_, id, tabindex, attribute, href, target, checked, hidden)
import Html.Events
import Table
import Utils.CommonFunctions exposing (isAlpha)


idAttr : String -> Html.Attribute msg
idAttr str =
    id (String.filter isAlpha str)


dataTarget : String -> Html.Attribute msg
dataTarget val =
    attribute "data-target" val


standardTheadHelp : ( String, Table.Status, Html.Attribute msg ) -> Html msg
standardTheadHelp ( name, status, onClick ) =
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
    in
        th [ headerColumnClass ] [ headerContent ]


standardThead : List ( String, Table.Status, Html.Attribute msg ) -> Table.HtmlDetails msg
standardThead headers =
    Table.HtmlDetails [ class "e-gridheader e-columnheader e-hidelines" ] (List.map standardTheadHelp headers)


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
                [ input [ type_ "checkbox", checked isChecked ] []
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


hrefColumnExtra : String -> (data -> String) -> String -> msg -> Table.Column data msg
hrefColumnExtra name hrefText toHref event =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> hrefColumnCellExtra (hrefText data) toHref event
        , sorter = Table.unsortable
        }


hrefColumnCell : String -> String -> Table.HtmlDetails msg
hrefColumnCell displayText url =
    Table.HtmlDetails []
        [ a [ href url, target "_blank" ] [ text displayText ]
        ]


hrefColumnCellExtra : String -> String -> msg -> Table.HtmlDetails msg
hrefColumnCellExtra displayText url event =
    Table.HtmlDetails []
        [ a [ href url, Html.Events.onClick event ] [ text displayText ]
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
