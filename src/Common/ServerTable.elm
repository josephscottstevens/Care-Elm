port module Common.ServerTable
    exposing
        ( GridOperations
        , ServerData
        , Config
        , Filter
        , Column
        , Operator(..)
        , Control(..)
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
        , htmlColumn
        , updateFromServer
        , decodeGridOperations
        , encodeGridOperations
        , initFilter
        , updateFilters
        , updateFilter
        )

import Html exposing (Html, div, table, th, td, tr, thead, tbody, text, button, ul, li, a, span, input)
import Html.Attributes exposing (class, id, style, type_, target, colspan, classList, href, disabled, checked)
import Html.Events as Events
import Common.Functions as Functions
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


port initFilters : List Filter -> Cmd msg


port updateFilters : (List Filter -> msg) -> Sub msg


type Operator
    = Equals
    | Between


type Control
    = TextControl
    | DateControl
    | DateTimeControl
    | NoControl
    | CheckBoxControl
    | FilterIsNewControl
    | Last60MonthsControl


getControlString : Control -> String
getControlString control =
    case control of
        TextControl ->
            "text"

        DateControl ->
            "date"

        DateTimeControl ->
            "datetime"

        NoControl ->
            "none"

        CheckBoxControl ->
            "checkbox"

        FilterIsNewControl ->
            "filterIsNew"

        Last60MonthsControl ->
            "last60Months"


getOperatorString : Operator -> String
getOperatorString operator =
    case operator of
        Equals ->
            "equals"

        Between ->
            "between"


type alias Filter =
    { name : String
    , controlType : String
    , value : String
    , value2 : String
    , expression : String
    }


buildFilter : List (Column data msg) -> List Filter
buildFilter columns =
    columns
        |> List.map
            (\column ->
                let
                    defaultFilter control operator =
                        { name = getColumnName column
                        , controlType = getControlString control
                        , value = ""
                        , value2 = ""
                        , expression = getOperatorString operator
                        }
                in
                    case column of
                        IntColumn _ _ _ ->
                            defaultFilter TextControl Equals

                        StringColumn _ _ _ ->
                            defaultFilter TextControl Equals

                        DateTimeColumn _ _ _ ->
                            defaultFilter DateTimeControl Equals

                        DateColumn _ _ _ ->
                            defaultFilter DateControl Equals

                        HrefColumn _ _ _ _ ->
                            defaultFilter NoControl Equals

                        HrefColumnExtra _ _ ->
                            defaultFilter NoControl Equals

                        CheckColumn _ _ _ ->
                            defaultFilter CheckBoxControl Equals

                        DropdownColumn _ ->
                            defaultFilter NoControl Equals

                        HtmlColumn _ _ _ control operator ->
                            defaultFilter control operator
            )


initFilter : List (Column data msg) -> Cmd msg
initFilter columns =
    buildFilter columns
        |> initFilters


updateFilter : List Filter -> GridOperations data msg -> GridOperations data msg
updateFilter filters gridOperations =
    { gridOperations | filters = filters }



-- Data Types


type alias GridOperations data msg =
    { selectedId : Maybe Int
    , openDropdownId : Maybe Int
    , skip : Int
    , pageSize : Int
    , rowsPerPage : Int
    , totalRows : Int
    , sortField : Maybe String
    , sortAscending : Bool
    , filters : List Filter
    , domTableId : String
    , toolbar : List ( String, msg )
    , columns : List (Column data msg)
    }


type alias ServerData =
    { skip : Int
    , pageSize : Int
    , rowsPerPage : Int
    , totalRows : Int
    , sortField : Maybe String
    , sortAscending : Bool
    }


type Page
    = First
    | Previous
    | PreviousBlock
    | Index Int
    | NextBlock
    | Next
    | Last


type alias Config data msg =
    { sortField : Maybe String
    , domTableId : String
    , toolbar : List ( String, msg )
    , columns : List (Column data msg)
    }


init : Config data msg -> GridOperations data msg
init config =
    { selectedId = Nothing
    , openDropdownId = Nothing
    , skip = 0
    , pageSize = 20
    , rowsPerPage = 10
    , totalRows = 0
    , sortField = config.sortField
    , sortAscending = False
    , filters = buildFilter config.columns
    , domTableId = config.domTableId
    , toolbar = config.toolbar
    , columns = config.columns
    }


type Column data msg
    = IntColumn String (data -> Maybe Int) String
    | StringColumn String (data -> Maybe String) String
    | DateTimeColumn String (data -> Maybe String) String
    | DateColumn String (data -> Maybe String) String
    | HrefColumn String String (data -> Maybe String) String
    | HrefColumnExtra String (data -> Html msg)
    | CheckColumn String (data -> Bool) String
    | DropdownColumn (List ( String, String, Int -> msg ))
    | HtmlColumn String (data -> Maybe String) String Control Operator


intColumn : String -> (data -> Maybe Int) -> String -> Column data msg
intColumn displayText data fieldName =
    IntColumn displayText data fieldName


stringColumn : String -> (data -> Maybe String) -> String -> Column data msg
stringColumn displayText data fieldName =
    StringColumn displayText data fieldName


dateTimeColumn : String -> (data -> Maybe String) -> String -> Column data msg
dateTimeColumn displayText data fieldName =
    DateTimeColumn displayText data fieldName


dateColumn : String -> (data -> Maybe String) -> String -> Column data msg
dateColumn displayText data fieldName =
    DateColumn displayText data fieldName


hrefColumn : String -> String -> (data -> Maybe String) -> String -> Column data msg
hrefColumn displayText displayStr data fieldName =
    HrefColumn displayText displayStr data fieldName


hrefColumnExtra : String -> (data -> Html msg) -> Column data msg
hrefColumnExtra displayText toNode =
    HrefColumnExtra displayText toNode


checkColumn : String -> (data -> Bool) -> String -> Column data msg
checkColumn displayText data fieldName =
    CheckColumn displayText data fieldName


dropdownColumn : List ( String, String, Int -> msg ) -> Column data msg
dropdownColumn items =
    DropdownColumn items


htmlColumn : String -> (data -> Maybe String) -> String -> Control -> Operator -> Column data msg
htmlColumn displayText data fieldName control operator =
    HtmlColumn displayText data fieldName control operator



-- VIEW


view : GridOperations data msg -> (GridOperations data msg -> msg) -> List data -> Maybe (Html msg) -> Html msg
view gridOperations toMsg rows maybeCustomRow =
    div [ class "e-grid e-js e-waitingpopup" ]
        [ viewToolbar gridOperations.toolbar
        , table [ id gridOperations.domTableId, class "e-table", style [ ( "border-collapse", "collapse" ) ] ]
            [ thead [ class "e-gridheader e-columnheader e-hidelines" ]
                [ tr [] (List.map (viewTh gridOperations toMsg) gridOperations.columns)
                , tr [] (List.map viewThFilter gridOperations.columns)
                ]
            , tbody []
                (viewTr gridOperations toMsg rows maybeCustomRow)
            ]
        , pagingView gridOperations toMsg
        ]


viewTr : GridOperations data msg -> (GridOperations data msg -> msg) -> List data -> Maybe (Html msg) -> List (Html msg)
viewTr gridOperations toMsg rows maybeCustomRow =
    let
        selectedStyle idx =
            style
                (if Just idx == gridOperations.selectedId then
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
                , selectedStyle ctr
                ]
                (List.map (viewTd ctr gridOperations toMsg row) gridOperations.columns)

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
                    [ td [ colspan (List.length gridOperations.columns), customCellStyle ]
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


viewTd : Int -> GridOperations data msg -> (GridOperations data msg -> msg) -> data -> Column data msg -> Html msg
viewTd idx gridOperations toMsg row column =
    let
        tdClass =
            classList
                [ ( "e-gridtooltip", True )
                , ( "e-active", Just idx == gridOperations.selectedId )
                ]

        tdStyle =
            style [ ( "padding-left", "8.4px" ) ]

        tdClick =
            case column of
                DropdownColumn _ ->
                    disabled False

                _ ->
                    Events.onClick (toMsg { gridOperations | selectedId = Just idx })
    in
        td [ tdClass, tdStyle, tdClick ]
            [ case column of
                IntColumn _ dataToInt _ ->
                    text (Functions.defaultIntToString (dataToInt row))

                StringColumn _ dataToString _ ->
                    text (Maybe.withDefault "" (dataToString row))

                DateTimeColumn _ dataToString _ ->
                    text (Functions.defaultDateTime (dataToString row))

                DateColumn _ dataToString _ ->
                    text (Functions.defaultDate (dataToString row))

                HrefColumn _ displayText dataToString _ ->
                    --TODO, how do I want to display empty? I think.. it is hide the href, not go to an empty url right?
                    a [ href (Maybe.withDefault "" (dataToString row)), target "_blank" ] [ text displayText ]

                HrefColumnExtra _ toNode ->
                    toNode row

                CheckColumn _ dataToString _ ->
                    div [ class "e-checkcell" ]
                        [ div [ class "e-checkcelldiv", style [ ( "text-align", "center" ) ] ]
                            [ input [ type_ "checkbox", disabled True, checked (dataToString row) ] []
                            ]
                        ]

                DropdownColumn dropDownItems ->
                    rowDropDownDiv idx gridOperations toMsg dropDownItems

                HtmlColumn _ dataToString _ _ _ ->
                    textHtml (Maybe.withDefault "" (dataToString row))
            ]


viewTh : GridOperations data msg -> (GridOperations data msg -> msg) -> Column data msg -> Html msg
viewTh gridOperations toMsg column =
    let
        name =
            getColumnName column

        displayValue =
            getColumnDisplayValue column

        headerContent =
            case gridOperations.sortField of
                Just t ->
                    if t == name then
                        if gridOperations.sortAscending then
                            [ text displayValue, span [ class "e-icon e-ascending e-rarrowup-2x" ] [] ]
                        else
                            [ text displayValue, span [ class "e-icon e-ascending e-rarrowdown-2x" ] [] ]
                    else
                        [ text displayValue ]

                Nothing ->
                    [ text displayValue ]

        newSortDirection =
            case gridOperations.sortField of
                Just _ ->
                    (not gridOperations.sortAscending)

                Nothing ->
                    gridOperations.sortAscending

        sortClick =
            Events.onClick (toMsg { gridOperations | sortAscending = newSortDirection, sortField = Just name })
    in
        th [ class ("e-headercell e-default " ++ name), sortClick ]
            [ div [ class "e-headercelldiv e-gridtooltip" ] headerContent
            ]


viewThFilter : Column data msg -> Html msg
viewThFilter column =
    th [ class "e-filterbarcell e-fltrtemp" ]
        [ div [ class "e-filterdiv e-fltrtempdiv" ]
            [ input [ id (getColumnName column ++ "_Id") ] []
            ]
        ]


textHtml : String -> Html msg
textHtml t =
    div
        [ Encode.string t
            |> Html.Attributes.property "innerHTML"
        ]
        []


getColumnDisplayValue : Column data msg -> String
getColumnDisplayValue column =
    case column of
        IntColumn displayText _ _ ->
            displayText

        StringColumn displayText _ _ ->
            displayText

        DateTimeColumn displayText _ _ ->
            displayText

        DateColumn displayText _ _ ->
            displayText

        HrefColumn displayText _ _ _ ->
            displayText

        HrefColumnExtra displayText _ ->
            displayText

        CheckColumn displayText _ _ ->
            displayText

        DropdownColumn _ ->
            ""

        HtmlColumn displayText _ _ _ _ ->
            displayText


getColumnName : Column data msg -> String
getColumnName column =
    case column of
        IntColumn _ _ name ->
            name

        StringColumn _ _ name ->
            name

        DateTimeColumn _ _ name ->
            name

        DateColumn _ _ name ->
            name

        HrefColumn _ _ _ name ->
            name

        HrefColumnExtra _ _ ->
            ""

        CheckColumn _ _ name ->
            name

        DropdownColumn _ ->
            ""

        HtmlColumn _ _ name _ _ ->
            name



-- Custom


rowDropDownDiv : Int -> GridOperations data msg -> (GridOperations data msg -> msg) -> List ( String, String, Int -> msg ) -> Html msg
rowDropDownDiv idx gridOperations toMsg dropDownItems =
    let
        dropClickEvent event =
            Events.onClick (event idx)

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
                    if idx == t then
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
                    Events.onClick (toMsg { gridOperations | openDropdownId = Just idx })

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


setPagingState : GridOperations data msg -> (GridOperations data msg -> msg) -> Page -> Html.Attribute msg
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


pagingView : GridOperations data msg -> (GridOperations data msg -> msg) -> Html msg
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


updateFromServer : ServerData -> GridOperations data msg -> GridOperations data msg
updateFromServer serverData dt =
    { dt
        | skip = serverData.skip
        , pageSize = serverData.pageSize
        , rowsPerPage = serverData.rowsPerPage
        , totalRows = serverData.totalRows
        , sortField = serverData.sortField
        , sortAscending = serverData.sortAscending
    }


encodeFilter : Filter -> Encode.Value
encodeFilter filter =
    Encode.object
        [ ( "name", Encode.string filter.name )
        , ( "controlType", Encode.string filter.controlType )
        , ( "value", Encode.string filter.value )
        , ( "value2", Encode.string filter.value2 )
        , ( "expression", Encode.string filter.expression )
        ]


encodeGridOperations : GridOperations data msg -> Encode.Value
encodeGridOperations gridOperations =
    Encode.object
        [ ( "SelectedId", Functions.maybeVal Encode.int gridOperations.selectedId )
        , ( "OpenDropdownId", Functions.maybeVal Encode.int gridOperations.openDropdownId )
        , ( "Skip", Encode.int gridOperations.skip )
        , ( "PageSize", Encode.int gridOperations.pageSize )
        , ( "RowsPerPage", Encode.int gridOperations.rowsPerPage )
        , ( "TotalRows", Encode.int gridOperations.totalRows )
        , ( "SortField", Functions.maybeVal Encode.string gridOperations.sortField )
        , ( "SortAscending", Encode.bool gridOperations.sortAscending )
        , ( "filters", Encode.list (List.map encodeFilter gridOperations.filters) )
        ]


decodeGridOperations : Decode.Decoder ServerData
decodeGridOperations =
    Pipeline.decode ServerData
        |> Pipeline.required "Skip" Decode.int
        |> Pipeline.required "PageSize" Decode.int
        |> Pipeline.required "RowsPerPage" Decode.int
        |> Pipeline.required "TotalRows" Decode.int
        |> Pipeline.required "SortField" (Decode.maybe Decode.string)
        |> Pipeline.required "SortAscending" Decode.bool
