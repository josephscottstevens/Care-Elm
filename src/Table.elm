module Table exposing (..)

import Html exposing (Html, Attribute, div, table, th, td, tr, thead, tbody, text, button, ul, li, a, span)
import Html.Attributes exposing (class, id, style, type_, target, colspan)
import Html.Events as Events


-- Data Types


type alias State =
    { selectedId : Maybe Int
    , isReversed : Bool
    , reversedId : Maybe Int
    , openDropdownId : Maybe Int
    }


init : State
init =
    { selectedId = Nothing
    , isReversed = False
    , reversedId = Nothing
    , openDropdownId = Nothing
    }


type alias Row msg =
    { columns : List (Column msg)
    , rowId : Int
    }


type Column msg
    = StringColumn String String
    | DropdownColumn (List ( String, String, Html.Attribute msg ))


type alias Config msg =
    { domTableId : String
    , headers : List String
    , toolbar : List ( String, msg )
    , toMsg : State -> msg
    }



-- stringColumn : String -> String -> Column msg
-- stringColumn name data =
--     StringColumn name data
-- dropdownColumn : State -> (State -> msg) -> List ( String, String, Html.Attribute msg ) -> Column msg
-- dropdownColumn state toMsg dropDownItems =
--     { name = "dropdownColumn"
--     , node = rowDropDownDiv state toMsg dropDownItems
--     }
-- customColumn : String -> Html msg -> Column msg
-- customColumn name toNode =
--     { name = name
--     , node = toNode
--     }
-- VIEW


view : State -> List (Row msg) -> Config msg -> Maybe (Html msg) -> Html msg
view state rows config maybeCustomRow =
    div [ class "e-grid e-js e-waitingpopup" ]
        [ viewToolbar config.toolbar
        , table [ id config.domTableId, style [ ( "width", "100%" ) ] ]
            [ thead [ class "e-gridheader e-columnheader e-hidelines" ]
                (List.map viewTh config.headers)
            , tbody []
                (viewTr state rows config maybeCustomRow)
            ]
        ]


viewTr : State -> List (Row msg) -> Config msg -> Maybe (Html msg) -> List (Html msg)
viewTr state rows config maybeCustomRow =
    let
        selectedStyle row =
            style
                (if Just row.rowId == state.selectedId then
                    [ ( "background-color", "#66aaff" )
                    , ( "background", "#66aaff" )
                    ]
                 else
                    [ ( "", "" ) ]
                )

        standardTr row =
            tr [ selectedStyle row ] (List.map (viewTd state row config) row.columns)

        inlineStyle =
            style
                [ ( "background-color", "white" )
                , ( "padding-top", "5px" )
                , ( "margin-left", "5px" )
                ]
    in
        case maybeCustomRow of
            Just customRow ->
                (tr []
                    [ td [ colspan 4, inlineStyle ]
                        [ customRow
                        ]
                    ]
                )
                    :: List.map standardTr rows

            Nothing ->
                List.map standardTr rows


viewTh : String -> Html msg
viewTh name =
    th [ class ("e-columnheader e-default e-filterbarcell " ++ name) ]
        [ div [ class "e-headercelldiv e-gridtooltip headerColumn" ] [ text name ]
        ]


viewTd : State -> Row msg -> Config msg -> Column msg -> Html msg
viewTd state row config column =
    td []
        [ case column of
            StringColumn name data ->
                text data

            DropdownColumn dropDownItems ->
                rowDropDownDiv state config.toMsg row dropDownItems
        ]



-- Custom


rowDropDownDiv : State -> (State -> msg) -> Row msg -> List ( String, String, Html.Attribute msg ) -> Html msg
rowDropDownDiv state toMsg row dropDownItems =
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
            case state.openDropdownId of
                Just t ->
                    if row.rowId == t then
                        [ ul [ class "e-menu e-js e-widget e-box e-separator" ]
                            (List.map dropDownMenuItem dropDownItems)
                        ]
                    else
                        []

                Nothing ->
                    []

        btnClass =
            class "btn btn-sm btn-default fa fa-angle-down btn-context-menu editDropDown"

        btnStyle =
            style [ ( "position", "relative" ) ]

        btnClick =
            case state.openDropdownId of
                Just _ ->
                    Events.onClick (toMsg { state | openDropdownId = Nothing })

                Nothing ->
                    Events.onClick (toMsg { state | openDropdownId = Just row.rowId })

        btnBlur =
            Events.onBlur (toMsg { state | openDropdownId = Nothing })
    in
        div []
            [ div [ style [ ( "text-align", "right" ) ] ]
                [ button [ type_ "button", btnClass, btnClick, btnBlur, btnStyle ]
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
