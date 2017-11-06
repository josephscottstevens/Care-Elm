module Utils.CommonGrid exposing (..)

import Html exposing (Html, text, div, program, button, input, span, th, li, ul, a, label)
import Html.Attributes exposing (style, class, type_, id, value, tabindex, for)
import Html.Events exposing (onInput)
import Table
import Char exposing (isLower, isUpper)


isAlpha : Char -> Bool
isAlpha char =
    isLower char || isUpper char


forId : String -> Html.Attribute msg
forId str =
    for (String.filter isAlpha str)


idAttr : String -> Html.Attribute msg
idAttr str =
    id (String.filter isAlpha str)


defaultString : Maybe String -> String
defaultString str =
    case str of
        Just t ->
            t

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



--No lie, really... really don't understand how\why this works


simpleRowAttrs : (a -> Int) -> a -> List (Html.Attribute msg)
simpleRowAttrs toInt =
    \data -> viewData (toInt data)


viewData : Int -> List (Html.Attribute msg)
viewData int =
    if int % 2 == 0 then
        [ class "e-row" ]
    else
        [ class "e-alt_row" ]


dropDownMenuItem : ( String, String, Html.Attribute msg ) -> Html msg
dropDownMenuItem ( iconClass, displayText, event ) =
    li [ class "e-content e-list" ]
        [ a [ class "e-menulink", event ]
            [ text displayText
            , span [ class ("e-gridcontext e-icon " ++ iconClass) ] []
            ]
        ]


dropDownMenu : List ( String, String, Html.Attribute msg ) -> Html msg
dropDownMenu dropDownMenuItems =
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


buildDropDown : List ( String, String, Html.Attribute msg ) -> Bool -> Html.Attribute msg -> Table.HtmlDetails msg
buildDropDown dropDownItems dropDownOpen toggleEvent =
    let
        dropDownList =
            case dropDownOpen of
                False ->
                    div [] []

                True ->
                    (dropDownMenu dropDownItems)
    in
        Table.HtmlDetails []
            [ div [ style [ ( "text-align", "right" ) ] ]
                [ button [ class "btn btn-sm btn-default fa fa-angle-down btn-context-menu", toggleEvent ] []
                , dropDownList
                ]
            ]


inputCommonWithType : (List (Html.Attribute msg) -> List a -> Html msg) -> String -> String -> String -> (String -> msg) -> Bool -> Html msg
inputCommonWithType control controlType displayText inputValue event isRequired =
    let
        isRequiredStr =
            if isRequired then
                " required"
            else
                ""
    in
        div [ class "form-group" ]
            [ label [ class ("col-sm-2 col-md-2 col-lg-2 control-label" ++ isRequiredStr), forId displayText ] [ text displayText ]
            , div [ class "col-sm-8 col-md-5 col-lg-4" ]
                [ control [ type_ controlType, class "e-textbox", idAttr displayText, value inputValue, onInput event ] [] ]
            ]


inputCommon : (List (Html.Attribute msg) -> List a -> Html msg) -> String -> String -> (String -> msg) -> Bool -> Html msg
inputCommon control displayText inputValue event isRequired =
    inputCommonWithType control "text" displayText inputValue event isRequired
