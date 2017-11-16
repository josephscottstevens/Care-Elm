module Utils.CommonGrid exposing (..)

import Html exposing (Html, text, div, button, input, span, th, li, ul, a)
import Html.Attributes exposing (style, class, type_, id, tabindex, attribute, href, target, checked)
import Html.Events
import Table
import Utils.CommonTypes exposing (DropDownState, FilterState)
import Json.Decode as Json


onInput : String -> (FilterState -> msg) -> Html.Attribute msg
onInput name handler =
    Html.Events.on "input" <| Json.map handler <| Json.map2 Utils.CommonTypes.FilterState (Json.succeed name) (Json.at [ "target", "value" ] Json.string)


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

        filterContent =
            if name == "" then
                div [] []
            else
                div [ class "e-filterdiv e-fltrinputdiv" ]
                    [ input [ class "e-ejinputtext e-filtertext", style [ ( "margin-bottom", "5px" ), ( "margin-top", "15px" ) ], onInput name event ] []
                    ]
    in
        th [ headerColumnClass ] [ headerContent, filterContent ]


standardThead : (FilterState -> msg) -> List ( String, Table.Status, Html.Attribute msg ) -> Table.HtmlDetails msg
standardThead msg headers =
    Table.HtmlDetails [ class "e-gridheader e-textover e-hidelines" ] (List.map (standardTheadHelp msg) headers)


standardTableAttrs : String -> List (Html.Attribute msg)
standardTableAttrs idValue =
    [ id idValue, class "", style [ ( "width", "100%" ) ] ]


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
        [ a [ href url ] [ text displayText ]
        ]


hrefColumnCellExtra : String -> String -> msg -> Table.HtmlDetails msg
hrefColumnCellExtra displayText url event =
    Table.HtmlDetails []
        [ a [ href url, Html.Events.onClick event ] [ text displayText ]
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


rowDropDownDiv : data -> Table.HtmlDetails msg
rowDropDownDiv data =
    Table.HtmlDetails []
        [ div [ style [ ( "text-align", "right" ) ] ]
            [ button [ type_ "button", class "btn btn-sm btn-default fa fa-angle-down btn-context-menu editDropDown", dataTarget (toString data) ] []
            ]
        ]


rowDropDown : Table.Column { b | id : a } msg
rowDropDown =
    Table.veryCustomColumn
        { name = ""
        , viewData = rowDropDownDiv << .id
        , sorter = Table.unsortable
        }


editDropDownDiv : List ( String, String, Html.Attribute msg ) -> DropDownState -> Html msg
editDropDownDiv dropDownItems dropDownState =
    div [ id "editButtonMenu", dropDownMenuStyle dropDownState ]
        [ ul [ class "e-menu e-js e-widget e-box e-separator", tabindex 0 ]
            (List.map dropDownMenuItem dropDownItems)
        ]



-- (24 * maxnumber of elements) + 2
-- currently 6 items, so , set max height to 146


dropDownMenuStyle : DropDownState -> Html.Attribute msg
dropDownMenuStyle dropDownState =
    style
        [ ( "z-index", "5000" )
        , ( "position", "absolute" )
        , ( "display", "block" )
        , ( "left", toString dropDownState.x ++ "px" )
        , ( "top", toString dropDownState.y ++ "px" )
        , ( "height", "146px" )
        ]


dropDownMenuItem : ( String, String, Html.Attribute msg ) -> Html msg
dropDownMenuItem ( iconClass, displayText, event ) =
    li [ class "e-content e-list" ]
        [ a [ class "e-menulink", event, target "_blank" ]
            [ text displayText
            , span [ class ("e-gridcontext e-icon " ++ iconClass) ] []
            ]
        ]
