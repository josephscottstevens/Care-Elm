module Utils.CommonGrid exposing (..)

import Html exposing (Html, text, div, program, button, input, span, th, li, ul, a, label)
import Html.Attributes exposing (style, class, type_, id, value, tabindex, for, attribute, href, target, checked)
import Table
import Utils.CommonTypes exposing (DropDownState)


dataTarget : String -> Html.Attribute msg
dataTarget val =
    attribute "data-target" val


defaultTheadHelp : ( String, Table.Status, Html.Attribute msg ) -> Html msg
defaultTheadHelp ( name, status, onClick ) =
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


defaultThead : List ( String, Table.Status, Html.Attribute msg ) -> Table.HtmlDetails msg
defaultThead headers =
    Table.HtmlDetails [ class "e-gridheader e-textover e-hidelines" ] (List.map defaultTheadHelp headers)


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


hrefColumnCell : String -> String -> Table.HtmlDetails msg
hrefColumnCell displayText url =
    Table.HtmlDetails []
        [ a [ href url ] [ text displayText ]
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


defaultTableAttributes : List (Html.Attribute msg)
defaultTableAttributes =
    [ class "e-grid e-js e-waitingpopup", style [ ( "width", "100%" ) ] ]


standardCustomizations : Table.Customizations data msg
standardCustomizations =
    let
        t =
            Table.defaultCustomizations
    in
        { t
            | tableAttrs = defaultTableAttributes
            , thead = defaultThead
        }


defaultRowAttrs : (a -> Int) -> a -> List (Html.Attribute msg)
defaultRowAttrs toInt =
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
