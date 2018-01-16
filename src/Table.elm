module Table exposing (..)

import Html exposing (Html, Attribute, div, table, th, td, tr, thead, tbody, text, button, ul, li, a, span)
import Html.Attributes exposing (class, id, style, type_, target, colspan)
import Html.Events as Events


-- Data Types


type alias Row msg =
    { columns : List (Column msg)
    , rowId : Int
    }



-- type AltRow msg
--     = StandardRow List (Column msg)
--     | CustomRow (Column msg)


type alias Column msg =
    { name : String
    , isReversed : Bool
    , node : Html msg
    }


type alias Config msg =
    { domTableId : String
    , headers : List String
    , toolbar : List ( String, msg )
    }


stringColumn : String -> String -> Column msg
stringColumn name data =
    { name = name
    , isReversed = False
    , node = text data
    }


dropdownColumn : Bool -> Html.Attribute msg -> List ( String, String, Html.Attribute msg ) -> Column msg
dropdownColumn isVisible event dropDownItems =
    { name = "dropdownColumn"
    , isReversed = False
    , node = rowDropDownDiv isVisible event dropDownItems
    }


customColumn : String -> Html msg -> Column msg
customColumn name toNode =
    { name = name
    , isReversed = False
    , node = toNode
    }



-- VIEW


view : List (Row msg) -> Config msg -> Maybe ( Int, Html msg ) -> Html msg
view rows config maybeCustomRow =
    div [ class "e-grid e-js e-waitingpopup" ]
        [ viewToolbar config.toolbar
        , table [ id config.domTableId, style [ ( "width", "100%" ) ] ]
            [ thead [ class "e-gridheader e-columnheader e-hidelines" ]
                (List.map viewTh config.headers)
            , tbody []
                (List.map (viewTr maybeCustomRow) rows)
            ]
        ]


viewTr : Maybe ( Int, Html msg ) -> Row msg -> Html msg
viewTr maybeCustomRow row =
    let
        standardTr =
            tr [] (List.map viewTd row.columns)

        inlineStyle =
            style
                [ ( "background-color", "white" )
                , ( "padding-top", "5px" )
                , ( "margin-left", "5px" )
                ]
    in
        case maybeCustomRow of
            Just ( rowId, customRow ) ->
                if rowId == row.rowId then
                    tr []
                        [ td [ colspan 4, inlineStyle ]
                            [ customRow
                            ]
                        ]
                else
                    standardTr

            Nothing ->
                standardTr


viewTh : String -> Html msg
viewTh name =
    th [ class ("e-columnheader e-default e-filterbarcell " ++ name) ]
        [ div [ class "e-headercelldiv e-gridtooltip headerColumn" ] [ text name ]
        ]


viewTd : Column msg -> Html msg
viewTd column =
    td [] [ column.node ]



-- Custom


rowDropDownDiv : Bool -> Html.Attribute msg -> List ( String, String, Html.Attribute msg ) -> Html msg
rowDropDownDiv isVisible event dropDownItems =
    let
        dropDownMenuItem : ( String, String, Html.Attribute msg ) -> Html msg
        dropDownMenuItem ( iconClass, displayText, event ) =
            li [ class "e-content e-list" ]
                [ a [ class "e-menulink", event, target "_blank" ]
                    [ text displayText
                    , span [ class ("e-gridcontext e-icon " ++ iconClass) ] []
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

        dropMenu =
            case isVisible of
                True ->
                    [ ul [ class "e-menu e-js e-widget e-box e-separator" ]
                        (List.map dropDownMenuItem dropDownItems)
                    ]

                False ->
                    []

        btnClass =
            class "btn btn-sm btn-default fa fa-angle-down btn-context-menu editDropDown"
    in
        div []
            [ div [ style [ ( "text-align", "right" ) ] ]
                [ button [ type_ "button", btnClass, event, style [ ( "position", "relative" ) ] ]
                    [ div [ id "editButtonMenu", dropDownMenuStyle ]
                        dropMenu
                    ]
                ]
            ]


viewToolbar : List ( String, msg ) -> Html msg
viewToolbar items =
    div [ class "e-gridtoolbar e-toolbar e-js e-widget e-box e-toolbarspan e-tooltip" ]
        [ ul [ class "e-ul e-horizontal" ]
            [ li [ class "e-tooltxt" ]
                (List.map toolbarHelper items)
            ]
        ]


toolbarHelper : ( String, msg ) -> Html msg
toolbarHelper ( iconStr, event ) =
    a [ class ("e-addnewitem e-toolbaricons e-icon " ++ iconStr), Events.onClick event, style [ ( "cursor", "pointer" ) ] ] []
