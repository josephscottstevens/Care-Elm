module Common.ServerTable
    exposing
        ( GridOperations
        , Column
        , Config
        , init
        , view
        , intColumn
        , stringColumn
        , dateTimeColumn
        , dropdownColumn
        , dateColumn
        , hrefColumn
        , hrefColumnExtra
        , checkColumn
        , decodeGridOperations
        , encodeGridOperations
        )

import Html exposing (Html, Attribute, div, table, th, td, tr, thead, tbody, text, button, ul, li, a, span, input)
import Html.Attributes exposing (class, id, style, type_, target, colspan, classList, href, disabled, checked)
import Html.Events as Events
import Common.Functions as Functions exposing (maybeVal)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


-- Data Types


type alias FilterField =
    { columnName : String
    , columnValue : String
    }


type alias GridOperations =
    { selectedId : Maybe Int
    , openDropdownId : Maybe Int
    , skip : Int
    , pageSize : Int
    , rowsPerPage : Int
    , totalRows : Int
    , sortField : Maybe String
    , sortAscending : Bool
    , filterFields : List FilterField
    }


type Page
    = First
    | Previous
    | PreviousBlock
    | Index Int
    | NextBlock
    | Next
    | Last


init : String -> GridOperations
init sortedColumnName =
    { selectedId = Nothing
    , openDropdownId = Nothing
    , skip = 0
    , pageSize = 20
    , rowsPerPage = 15
    , totalRows = -1
    , sortField = Just "DoB"
    , sortAscending = False
    , filterFields = []
    }


type Column data msg
    = IntColumn String ({ data | id : Int } -> Maybe Int) String String
    | StringColumn String ({ data | id : Int } -> Maybe String) String String
    | DateTimeColumn String ({ data | id : Int } -> Maybe String) String String
    | DateColumn String ({ data | id : Int } -> Maybe String) String String
    | HrefColumn String String ({ data | id : Int } -> Maybe String) String String
    | HrefColumnExtra String ({ data | id : Int } -> Html msg)
    | CheckColumn String ({ data | id : Int } -> Bool) String String
    | DropdownColumn (List ( String, String, Int -> msg ))


intColumn : String -> ({ data | id : Int } -> Maybe Int) -> Column data msg
intColumn name data =
    IntColumn name data name ""


stringColumn : String -> ({ data | id : Int } -> Maybe String) -> Column data msg
stringColumn name data =
    StringColumn name data name ""


dateTimeColumn : String -> ({ data | id : Int } -> Maybe String) -> Column data msg
dateTimeColumn name data =
    DateTimeColumn name data name ""


dateColumn : String -> ({ data | id : Int } -> Maybe String) -> Column data msg
dateColumn name data =
    DateColumn name data name ""


hrefColumn : String -> String -> ({ data | id : Int } -> Maybe String) -> Column data msg
hrefColumn name displayStr data =
    HrefColumn name displayStr data name ""


hrefColumnExtra : String -> ({ data | id : Int } -> Html msg) -> Column data msg
hrefColumnExtra name toNode =
    HrefColumnExtra name toNode


checkColumn : String -> ({ data | id : Int } -> Bool) -> Column data msg
checkColumn name data =
    CheckColumn name data name ""


dropdownColumn : List ( String, String, Int -> msg ) -> Column data msg
dropdownColumn items =
    DropdownColumn items


type alias Config data msg =
    { domTableId : String
    , toolbar : List ( String, msg )
    , toMsg : GridOperations -> msg
    , columns : List (Column { data | id : Int } msg)
    }


type Sorter data
    = None
    | IncOrDec (List { data | id : Int } -> List { data | id : Int })



-- VIEW


view : GridOperations -> List { data | id : Int } -> Config { data | id : Int } msg -> Maybe (Html msg) -> Html msg
view gridOperations rows config maybeCustomRow =
    div [ class "e-grid e-js e-waitingpopup" ]
        [ viewToolbar config.toolbar
        , table [ id config.domTableId, class "e-table", style [ ( "border-collapse", "collapse" ) ] ]
            [ thead [ class "e-gridheader e-columnheader e-hidelines" ]
                [ tr [] (List.map (viewTh gridOperations config) config.columns)
                , tr [] (List.map (viewThFilter gridOperations config) config.columns)
                ]
            , tbody []
                (viewTr gridOperations rows config maybeCustomRow)
            ]
        , pagingView gridOperations config.toMsg
        ]


viewTr : GridOperations -> List { data | id : Int } -> Config { data | id : Int } msg -> Maybe (Html msg) -> List (Html msg)
viewTr gridOperations rows config maybeCustomRow =
    let
        selectedStyle row =
            style
                (if Just row.id == gridOperations.selectedId then
                    [ ( "background-color", "#66aaff" )
                    , ( "background", "#66aaff" )
                    ]
                 else
                    [ ( "", "" ) ]
                )

        rowClass ctr =
            classList
                [ ( "e-row", ctr % 2 == 0 )
                , ( "e-alt_row", ctr % 2 == 1 )
                ]

        standardTr ctr row =
            tr
                [ rowClass ctr
                , selectedStyle row
                ]
                (List.map (viewTd gridOperations row config) config.columns)

        customRowStyle =
            if List.length rows == 0 then
                style []
            else
                style
                    [ ( "border-bottom-color", "#cecece" )
                    , ( "border-bottom-width", "1px" )
                    , ( "border-bottom-style", "solid" )
                    ]

        customCellStyle =
            style
                [ ( "background-color", "white" )
                , ( "padding-top", "10px" )
                , ( "margin-left", "5px" )
                ]
    in
        case maybeCustomRow of
            Just customRow ->
                tr [ customRowStyle ]
                    [ td [ colspan (List.length config.columns), customCellStyle ]
                        [ customRow
                        ]
                    ]
                    :: List.indexedMap standardTr rows

            Nothing ->
                if List.length rows == 0 then
                    [ tr []
                        [ td [] [ text "No records to display" ]
                        ]
                    ]
                else
                    List.indexedMap standardTr rows


viewTh : GridOperations -> Config { data | id : Int } msg -> Column { data | id : Int } msg -> Html msg
viewTh gridOperations config column =
    let
        name =
            getColumnName column

        headerContent =
            case gridOperations.sortField of
                Just t ->
                    if t == name then
                        if gridOperations.sortAscending then
                            [ text name, span [ class "e-icon e-ascending e-rarrowup-2x" ] [] ]
                        else
                            [ text name, span [ class "e-icon e-ascending e-rarrowdown-2x" ] [] ]
                    else
                        [ text name ]

                Nothing ->
                    [ text name ]

        newSortDirection =
            case gridOperations.sortField of
                Just t ->
                    (not gridOperations.sortAscending)

                Nothing ->
                    gridOperations.sortAscending

        sortClick =
            Events.onClick (config.toMsg { gridOperations | sortAscending = newSortDirection, sortField = Just name })
    in
        th [ class ("e-headercell e-default " ++ name), sortClick ]
            [ div [ class "e-headercelldiv e-gridtooltip" ] headerContent
            ]


viewThFilter : GridOperations -> Config { data | id : Int } msg -> Column { data | id : Int } msg -> Html msg
viewThFilter gridOperations config column =
    th [ class ("e-filterbarcell") ]
        [ div [ class "e-filterdiv e-fltrinputdiv" ]
            [ input [ class "e-ejinputtext e-filtertext" ] []
            , span [ class "e-cancel e-icon" ] []
            ]
        ]


viewTd : GridOperations -> { data | id : Int } -> Config { data | id : Int } msg -> Column { data | id : Int } msg -> Html msg
viewTd gridOperations row config column =
    let
        tdClass =
            classList
                [ ( "e-gridtooltip", True )
                , ( "e-active", Just row.id == gridOperations.selectedId )
                ]

        tdStyle =
            style [ ( "padding-left", "8.4px" ) ]

        tdClick =
            case column of
                DropdownColumn _ ->
                    disabled False

                _ ->
                    Events.onClick (config.toMsg { gridOperations | selectedId = Just row.id })
    in
        td [ tdClass, tdStyle, tdClick ]
            [ case column of
                IntColumn _ dataToInt _ _ ->
                    text (Functions.defaultIntToString (dataToInt row))

                StringColumn _ dataToString _ _ ->
                    text (Maybe.withDefault "" (dataToString row))

                DateTimeColumn _ dataToString _ _ ->
                    text (Functions.defaultDateTime (dataToString row))

                DateColumn _ dataToString _ _ ->
                    text (Functions.defaultDate (dataToString row))

                HrefColumn _ displayText dataToString _ _ ->
                    --TODO, how do I want to display empty? I think.. it is hide the href, not go to an empty url right?
                    a [ href (Maybe.withDefault "" (dataToString row)), target "_blank" ] [ text displayText ]

                HrefColumnExtra _ toNode ->
                    toNode row

                CheckColumn _ dataToString _ _ ->
                    div [ class "e-checkcell" ]
                        [ div [ class "e-checkcelldiv", style [ ( "text-align", "center" ) ] ]
                            [ input [ type_ "checkbox", disabled True, checked (dataToString row) ] []
                            ]
                        ]

                DropdownColumn dropDownItems ->
                    rowDropDownDiv gridOperations config.toMsg row dropDownItems
            ]


getColumnName : Column { data | id : Int } msg -> String
getColumnName column =
    case column of
        IntColumn name _ _ _ ->
            name

        StringColumn name _ _ _ ->
            name

        DateTimeColumn name _ _ _ ->
            name

        DateColumn name _ _ _ ->
            name

        HrefColumn name _ _ _ _ ->
            name

        HrefColumnExtra name _ ->
            name

        CheckColumn name _ _ _ ->
            name

        DropdownColumn _ ->
            ""



-- Custom


rowDropDownDiv : GridOperations -> (GridOperations -> msg) -> { data | id : Int } -> List ( String, String, Int -> msg ) -> Html msg
rowDropDownDiv gridOperations toMsg row dropDownItems =
    let
        dropClickEvent event =
            Events.onClick (event row.id)

        dropDownMenuItem : ( String, String, Int -> msg ) -> Html msg
        dropDownMenuItem ( iconClass, displayText, event ) =
            li [ class "e-content e-list", dropClickEvent event ]
                [ a [ class "e-menulink", target "_blank" ]
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
            case gridOperations.openDropdownId of
                Just t ->
                    if row.id == t then
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

        clickEvent =
            case gridOperations.openDropdownId of
                Just _ ->
                    Events.onClick (toMsg { gridOperations | openDropdownId = Nothing })

                Nothing ->
                    Events.onClick (toMsg { gridOperations | openDropdownId = Just row.id })

        blurEvent =
            Events.onBlur (toMsg { gridOperations | openDropdownId = Nothing })
    in
        div []
            [ div [ style [ ( "text-align", "right" ) ] ]
                [ button [ id "contextMenuButton", type_ "button", btnClass, clickEvent, blurEvent, btnStyle ]
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
    let
        iconStyle =
            if String.contains "e-disable" iconStr then
                style []
            else
                style [ ( "cursor", "pointer" ) ]

        iconClass =
            "e-addnewitem e-toolbaricons e-icon " ++ iconStr
    in
        a [ class iconClass, Events.onClick event, iconStyle ] []



-- paging


setPagingState : GridOperations -> (GridOperations -> msg) -> Page -> Html.Attribute msg
setPagingState gridOperations toMsg page =
    let
        newIndex =
            case page of
                First ->
                    0

                Previous ->
                    if gridOperations.skip > 0 then
                        gridOperations.skip - 1
                    else
                        0

                PreviousBlock ->
                    0

                Index t ->
                    t

                NextBlock ->
                    0

                Next ->
                    gridOperations.skip + 1

                Last ->
                    (gridOperations.totalRows // gridOperations.pageSize) - 1
    in
        Events.onClick (toMsg { gridOperations | skip = newIndex })


pagingView : GridOperations -> (GridOperations -> msg) -> Html msg
pagingView gridOperations toMsg =
    let
        totalPages =
            (gridOperations.totalRows // gridOperations.pageSize) - 1

        pagingStateClick page =
            setPagingState gridOperations toMsg page

        activeOrNot skip =
            let
                activeOrNotText =
                    if skip == gridOperations.skip then
                        "e-currentitem e-active"
                    else
                        "e-default"
            in
                div
                    [ class ("e-link e-numericitem e-spacing " ++ activeOrNotText), pagingStateClick (Index skip) ]
                    [ text (toString (skip + 1)) ]

        rng =
            List.range 0 totalPages
                |> List.drop ((gridOperations.skip // gridOperations.rowsPerPage) * gridOperations.rowsPerPage)
                |> List.take gridOperations.rowsPerPage
                |> List.map activeOrNot

        firstPageClass =
            if gridOperations.skip >= gridOperations.pageSize then
                "e-icon e-mediaback e-firstpage e-default"
            else
                "e-icon e-mediaback e-firstpagedisabled e-disable"

        leftPageClass =
            if gridOperations.skip > 0 then
                "e-icon e-arrowheadleft-2x e-prevpage e-default"
            else
                "e-icon e-arrowheadleft-2x e-prevpagedisabled e-disable"

        leftPageBlockClass =
            if gridOperations.skip >= gridOperations.rowsPerPage then
                "e-link e-spacing e-PP e-numericitem e-default"
            else
                "e-link e-nextprevitemdisabled e-disable e-spacing e-PP"

        rightPageBlockClass =
            if gridOperations.skip < totalPages - gridOperations.rowsPerPage then
                "e-link e-NP e-spacing e-numericitem e-default"
            else
                "e-link e-NP e-spacing e-nextprevitemdisabled e-disable"

        rightPageClass =
            if gridOperations.skip < totalPages then
                "e-nextpage e-icon e-arrowheadright-2x e-default"
            else
                "e-icon e-arrowheadright-2x e-nextpagedisabled e-disable"

        lastPageClass =
            if gridOperations.skip < totalPages - gridOperations.rowsPerPage then
                "e-lastpage e-icon e-mediaforward e-default"
            else
                "e-icon e-mediaforward e-animate e-lastpagedisabled e-disable"

        pagerText =
            let
                currentPageText =
                    toString (gridOperations.skip + 1)

                totalPagesText =
                    toString (totalPages + 1)

                totalItemsText =
                    toString gridOperations.totalRows
            in
                currentPageText ++ " of " ++ totalPagesText ++ " pages (" ++ totalItemsText ++ " items)"
    in
        div [ class "e-pager e-js e-pager" ]
            [ div [ class "e-pagercontainer" ]
                [ div [ class firstPageClass, pagingStateClick First ] []
                , div [ class leftPageClass, pagingStateClick Previous ] []
                , a [ class leftPageBlockClass, pagingStateClick PreviousBlock ] [ text "..." ]
                , div [ class "e-numericcontainer e-default" ] rng
                , a [ class rightPageBlockClass, pagingStateClick NextBlock ] [ text "..." ]
                , div [ class rightPageClass, pagingStateClick Next ] []
                , div [ class lastPageClass, pagingStateClick Last ] []
                ]
            , div [ class "e-parentmsgbar", style [ ( "text-align", "right" ) ] ]
                [ span [ class "e-pagermsg" ] [ text pagerText ]
                ]
            ]



--Server Stuff


encodeFilterField : FilterField -> Encode.Value
encodeFilterField filterField =
    Encode.object
        [ ( "ColumnName", Encode.string filterField.columnName )
        , ( "ColumnValue", Encode.string filterField.columnValue )
        ]


decodeFilterField : Decode.Decoder FilterField
decodeFilterField =
    Pipeline.decode FilterField
        |> Pipeline.required "ColumnName" Decode.string
        |> Pipeline.required "ColumnValue" Decode.string


encodeGridOperations : GridOperations -> Encode.Value
encodeGridOperations gridOperations =
    Encode.object
        [ ( "SelectedId", maybeVal Encode.int gridOperations.selectedId )
        , ( "OpenDropdownId", maybeVal Encode.int gridOperations.openDropdownId )
        , ( "Skip", Encode.int gridOperations.skip )
        , ( "PageSize", Encode.int gridOperations.pageSize )
        , ( "RowsPerPage", Encode.int gridOperations.rowsPerPage )
        , ( "TotalRows", Encode.int gridOperations.totalRows )
        , ( "SortField", maybeVal Encode.string gridOperations.sortField )
        , ( "SortAscending", Encode.bool gridOperations.sortAscending )
        , ( "FilterFields", Encode.list (List.map encodeFilterField gridOperations.filterFields) )
        ]


decodeGridOperations : Decode.Decoder GridOperations
decodeGridOperations =
    Pipeline.decode GridOperations
        |> Pipeline.required "SelectedId" (Decode.maybe Decode.int)
        |> Pipeline.required "OpenDropdownId" (Decode.maybe Decode.int)
        |> Pipeline.required "Skip" Decode.int
        |> Pipeline.required "PageSize" Decode.int
        |> Pipeline.required "RowsPerPage" Decode.int
        |> Pipeline.required "TotalRows" Decode.int
        |> Pipeline.required "SortField" (Decode.maybe Decode.string)
        |> Pipeline.required "SortAscending" Decode.bool
        |> Pipeline.required "FilterFields" (Decode.list decodeFilterField)
