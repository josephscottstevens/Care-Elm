module Common.Table
    exposing
        ( Column
        , Config
        , State
        , checkColumn
        , dateColumn
        , dateTimeColumn
        , dropdownColumn
        , hrefColumn
        , hrefColumnExtra
        , htmlColumn
        , init
        , intColumn
        , stringColumn
        , view
        )

import Common.Dates as Functions
import Common.Functions as Functions
import Html exposing (Html, a, button, div, input, li, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (checked, class, classList, colspan, disabled, href, id, style, target, type_)
import Html.Events as Events
import Json.Encode as Encode
import Time



-- Data Types


blockSize : Int
blockSize =
    15


type alias State =
    { selectedId : Maybe Int
    , openDropdownId : Maybe Int
    , pageIndex : Int
    , rowsPerPage : Int
    , sortField : String
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


init : String -> State
init sortedColumnName =
    { selectedId = Nothing
    , openDropdownId = Nothing
    , pageIndex = 0
    , rowsPerPage = 20
    , sortField = sortedColumnName
    , sortAscending = True
    }


type Column data msg
    = IntColumn String ({ data | id : Int } -> Maybe Int) (Sorter data)
    | StringColumn String ({ data | id : Int } -> Maybe String) (Sorter data)
    | DateTimeColumn String ({ data | id : Int } -> Maybe String) (Sorter data)
    | DateColumn String ({ data | id : Int } -> Maybe String) (Sorter data)
    | HrefColumn String String ({ data | id : Int } -> Maybe String) (Sorter data)
    | HrefColumnExtra String ({ data | id : Int } -> Html msg)
    | CheckColumn String ({ data | id : Int } -> Bool) (Sorter data)
    | DropdownColumn (List ( String, String, data -> msg ))
    | HtmlColumn String ({ data | id : Int } -> Maybe String) (Sorter data)


intColumn : String -> ({ data | id : Int } -> Maybe Int) -> Column data msg
intColumn name data =
    IntColumn name data (defaultIntSort data)


stringColumn : String -> ({ data | id : Int } -> Maybe String) -> Column data msg
stringColumn name data =
    StringColumn name data (defaultSort data)


dateTimeColumn : String -> ({ data | id : Int } -> Maybe String) -> Column data msg
dateTimeColumn name data =
    DateTimeColumn name data (defaultSort data)


dateColumn : String -> ({ data | id : Int } -> Maybe String) -> Column data msg
dateColumn name data =
    DateColumn name data (defaultSort data)


hrefColumn : String -> String -> ({ data | id : Int } -> Maybe String) -> Column data msg
hrefColumn url displayStr data =
    HrefColumn url displayStr data (defaultSort data)


hrefColumnExtra : String -> ({ data | id : Int } -> Html msg) -> Column data msg
hrefColumnExtra name toNode =
    HrefColumnExtra name toNode


checkColumn : String -> ({ data | id : Int } -> Bool) -> Column data msg
checkColumn str data =
    CheckColumn str data (defaultBoolSort data)


dropdownColumn : List ( String, String, data -> msg ) -> Column data msg
dropdownColumn items =
    DropdownColumn items


htmlColumn : String -> ({ data | id : Int } -> Maybe String) -> Column data msg
htmlColumn name data =
    HtmlColumn name data (defaultSort data)


type alias Config data msg =
    { domTableId : String
    , toolbar : List ( String, msg )
    , toMsg : State -> msg
    , columns : List (Column { data | id : Int } msg)
    }


type Sorter data
    = None
    | IncOrDec (List { data | id : Int } -> List { data | id : Int })



-- VIEW


view : State -> List { data | id : Int } -> Config { data | id : Int } msg -> Maybe (Html msg) -> Html msg
view state rows config maybeCustomRow =
    let
        sortedRows =
            sortBy state config.columns rows

        filteredRows =
            sortedRows
                |> List.drop (state.pageIndex * state.rowsPerPage)
                |> List.take state.rowsPerPage

        totalRows =
            List.length rows
    in
    div [ class "e-grid e-js e-waitingpopup" ]
        [ viewToolbar config.toolbar
        , table [ id config.domTableId, class "e-table", style "border-collapse" "collapse" ]
            [ thead [ class "e-gridheader e-columnheader e-hidelines" ]
                [ tr [] (List.map (viewTh state config) config.columns)

                -- This is for filters, this can come at a later time
                -- , tr [] (List.map (viewThFilter state config) config.columns)
                ]
            , tbody []
                (viewTr state filteredRows config maybeCustomRow)
            ]
        , pagingView state totalRows filteredRows config.toMsg
        ]


viewTr : State -> List { data | id : Int } -> Config { data | id : Int } msg -> Maybe (Html msg) -> List (Html msg)
viewTr state rows config maybeCustomRow =
    let
        selectedStyle row =
            if Just row.id == state.selectedId then
                [ style "background-color" "#66aaff"
                , style "background" "#66aaff"
                ]

            else
                [ style "" "" ]

        rowClass ctr =
            classList
                [ ( "e-row", modBy ctr 2 == 0 )
                , ( "e-alt_row", modBy ctr 2 == 1 )
                ]

        standardTr ctr row =
            tr
                ([ rowClass ctr
                 ]
                    ++ selectedStyle row
                )
                (List.map (viewTd state row config) config.columns)

        customRowStyle =
            if List.length rows == 0 then
                [ style "" "" ]

            else
                [ style "border-bottom-color" "#cecece"
                , style "border-bottom-width" "1px"
                , style "border-bottom-style" "solid"
                ]

        customCellStyle =
            [ style "background-color" "white"
            , style "padding-top" "10px"
            , style "margin-left" "5px"
            ]
    in
    case maybeCustomRow of
        Just customRow ->
            tr customRowStyle
                [ td ([ colspan (List.length config.columns) ] ++ customCellStyle)
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


viewTh : State -> Config { data | id : Int } msg -> Column { data | id : Int } msg -> Html msg
viewTh state config column =
    let
        name =
            getColumnName column

        headerContent =
            if state.sortField == name then
                if not state.sortAscending then
                    [ text name, span [ class "e-icon e-ascending e-rarrowup-2x" ] [] ]

                else
                    [ text name, span [ class "e-icon e-ascending e-rarrowdown-2x" ] [] ]

            else
                [ text name ]

        newSortDirection =
            not state.sortAscending

        sortClick =
            Events.onClick (config.toMsg { state | sortAscending = newSortDirection, sortField = name })
    in
    th [ class ("e-headercell e-default " ++ name), sortClick ]
        [ div [ class "e-headercelldiv e-gridtooltip" ] headerContent
        ]



-- viewThFilter : State -> Config { data | id : Int } msg -> Column { data | id : Int } msg -> Html msg
-- viewThFilter state config column =
--     th [ class "e-filterbarcell" ]
--         [ div [ class "e-filterdiv e-fltrinputdiv" ]
--             [ input [ class "e-ejinputtext e-filtertext" ] []
--             , span [ class "e-cancel e-icon" ] []
--             ]
--         ]


viewTd : State -> { data | id : Int } -> Config { data | id : Int } msg -> Column { data | id : Int } msg -> Html msg
viewTd state row config column =
    let
        tdClass =
            classList
                [ ( "e-gridtooltip", True )
                , ( "e-active", Just row.id == state.selectedId )
                ]

        tdStyle =
            style "padding-left" "8.4px"

        tdClick =
            case column of
                DropdownColumn _ ->
                    disabled False

                _ ->
                    Events.onClick (config.toMsg { state | selectedId = Just row.id })
    in
    td [ tdClass, tdStyle, tdClick ]
        [ case column of
            IntColumn _ dataToInt _ ->
                text (Functions.defaultIntToString (dataToInt row))

            StringColumn _ dataToString _ ->
                text (Maybe.withDefault "" (dataToString row))

            DateTimeColumn _ dataToString _ ->
                text ""

            -- TODO
            -- text (Functions.defaultDateTimeToString (dataToString row))
            DateColumn _ dataToString _ ->
                text ""

            -- TODO
            --text (Functions.defaultDateToString (dataToString row))
            HrefColumn _ displayText dataToString _ ->
                case dataToString row of
                    Just t ->
                        a [ href t, target "_blank" ]
                            [ text t ]

                    Nothing ->
                        text ""

            HrefColumnExtra _ toNode ->
                toNode row

            CheckColumn _ dataToString _ ->
                div [ class "e-checkcell" ]
                    [ div [ class "e-checkcelldiv", style "text-align" "center" ]
                        [ input [ type_ "checkbox", disabled True, checked (dataToString row) ] []
                        ]
                    ]

            DropdownColumn dropDownItems ->
                rowDropDownDiv state config.toMsg row dropDownItems

            HtmlColumn _ dataToString _ ->
                textHtml (Maybe.withDefault "" (dataToString row))
        ]


textHtml : String -> Html msg
textHtml t =
    div
        [ Encode.string t
            |> Html.Attributes.property "innerHTML"
        ]
        []


getColumnName : Column { data | id : Int } msg -> String
getColumnName column =
    case column of
        IntColumn name _ _ ->
            name

        StringColumn name _ _ ->
            name

        DateTimeColumn name _ _ ->
            name

        DateColumn name _ _ ->
            name

        HrefColumn name _ _ _ ->
            name

        HrefColumnExtra name _ ->
            name

        CheckColumn name _ _ ->
            name

        DropdownColumn _ ->
            ""

        HtmlColumn name _ _ ->
            name



-- Custom


rowDropDownDiv : State -> (State -> msg) -> { data | id : Int } -> List ( String, String, { data | id : Int } -> msg ) -> Html msg
rowDropDownDiv state toMsg row dropDownItems =
    let
        dropClickEvent event =
            Events.onClick (event row)

        dropDownMenuItem : ( String, String, { data | id : Int } -> msg ) -> Html msg
        dropDownMenuItem ( iconClass, displayText, event ) =
            li [ class "e-content e-list", dropClickEvent event ]
                [ a [ class "e-menulink", target "_blank" ]
                    [ text displayText
                    , span [ class ("e-gridcontext e-icon " ++ iconClass) ] []
                    ]
                ]

        dropDownMenuStyle : List (Html.Attribute msg)
        dropDownMenuStyle =
            [ style "z-index" "5000"
            , style "position" "absolute"
            , style "display" "block"
            , style "left" "-173px"
            , style "width" "178.74px"
            ]

        dropMenu =
            case state.openDropdownId of
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
            style "position" "relative"

        clickEvent =
            case state.openDropdownId of
                Just _ ->
                    Events.onClick (toMsg { state | openDropdownId = Nothing })

                Nothing ->
                    Events.onClick (toMsg { state | openDropdownId = Just row.id })

        blurEvent =
            Events.onBlur (toMsg { state | openDropdownId = Nothing })
    in
    div []
        [ div [ style "text-align" "right" ]
            [ button [ id "contextMenuButton", type_ "button", btnClass, clickEvent, blurEvent, btnStyle ]
                [ div ([ id "editButtonMenu" ] ++ dropDownMenuStyle)
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
                style "" ""

            else
                style "cursor" "pointer"

        iconClass =
            "e-addnewitem e-toolbaricons e-icon " ++ iconStr
    in
    a [ class iconClass, Events.onClick event, iconStyle, id "btnNewRecord" ] []



-- paging


setPagingState : State -> Int -> (State -> msg) -> Page -> Html.Attribute msg
setPagingState state totalRows toMsg page =
    let
        lastIndex =
            totalRows // state.rowsPerPage

        bounded t =
            if t > lastIndex then
                lastIndex

            else if t < 0 then
                0

            else
                t

        newIndex =
            case page of
                First ->
                    0

                Previous ->
                    bounded (state.pageIndex - 1)

                PreviousBlock ->
                    bounded (state.pageIndex - blockSize)

                Index t ->
                    bounded t

                NextBlock ->
                    bounded (state.pageIndex + blockSize)

                Next ->
                    bounded (state.pageIndex + 1)

                Last ->
                    lastIndex
    in
    Events.onClick (toMsg { state | pageIndex = newIndex })


pagingView : State -> Int -> List { data | id : Int } -> (State -> msg) -> Html msg
pagingView state totalRows rows toMsg =
    let
        lastIndex =
            totalRows // state.rowsPerPage

        pagingStateClick page =
            setPagingState state totalRows toMsg page

        activeOrNot pageIndex =
            let
                activeOrNotText =
                    if pageIndex == state.pageIndex then
                        "e-currentitem e-active"

                    else
                        "e-default"
            in
            div
                [ class ("e-link e-numericitem e-spacing " ++ activeOrNotText)
                , pagingStateClick (Index pageIndex)
                ]
                [ text (String.fromInt (pageIndex + 1)) ]

        rng =
            List.range 0 lastIndex
                |> List.drop ((state.pageIndex // blockSize) * blockSize)
                |> List.take blockSize
                |> List.map activeOrNot

        firstPageClass =
            if state.pageIndex > 1 then
                "e-icon e-mediaback e-firstpage e-default"

            else
                "e-icon e-mediaback e-firstpagedisabled e-disable"

        leftPageClass =
            if state.pageIndex > 0 then
                "e-icon e-arrowheadleft-2x e-prevpage e-default"

            else
                "e-icon e-arrowheadleft-2x e-prevpagedisabled e-disable"

        leftPageBlockClass =
            if state.pageIndex >= blockSize then
                "e-link e-spacing e-PP e-numericitem e-default"

            else
                "e-link e-nextprevitemdisabled e-disable e-spacing e-PP"

        rightPageBlockClass =
            if state.pageIndex < lastIndex - blockSize then
                "e-link e-NP e-spacing e-numericitem e-default"

            else
                "e-link e-NP e-spacing e-nextprevitemdisabled e-disable"

        rightPageClass =
            if state.pageIndex < lastIndex then
                "e-nextpage e-icon e-arrowheadright-2x e-default"

            else
                "e-icon e-arrowheadright-2x e-nextpagedisabled e-disable"

        lastPageClass =
            if state.pageIndex < lastIndex then
                "e-lastpage e-icon e-mediaforward e-default"

            else
                "e-icon e-mediaforward e-animate e-lastpagedisabled e-disable"

        pagerText =
            let
                currentPageText =
                    String.fromInt (state.pageIndex + 1)

                totalPagesText =
                    String.fromInt <|
                        if lastIndex < 1 then
                            1

                        else
                            lastIndex + 1

                totalItemsText =
                    String.fromInt totalRows
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
        , div [ class "e-parentmsgbar", style "text-align" "right" ]
            [ span [ class "e-pagermsg" ] [ text pagerText ]
            ]
        ]



-- Sorting


sortBy : State -> List (Column { data | id : Int } msg) -> List { data | id : Int } -> List { data | id : Int }
sortBy state columnData data =
    case findSorter state.sortField columnData of
        Nothing ->
            data

        Just sorter ->
            applySorter state.sortAscending sorter data


applySorter : Bool -> Sorter { data | id : Int } -> List { data | id : Int } -> List { data | id : Int }
applySorter isReversed sorter data =
    case sorter of
        None ->
            data

        IncOrDec sort ->
            if isReversed then
                List.reverse (sort data)

            else
                sort data


findSorter : String -> List (Column { data | id : Int } msg) -> Maybe (Sorter { data | id : Int })
findSorter selectedColumn columnData =
    case columnData of
        [] ->
            Nothing

        column :: remainingColumnData ->
            case column of
                IntColumn name _ sorter ->
                    if name == selectedColumn then
                        Just sorter

                    else
                        findSorter selectedColumn remainingColumnData

                StringColumn name _ sorter ->
                    if name == selectedColumn then
                        Just sorter

                    else
                        findSorter selectedColumn remainingColumnData

                DateTimeColumn name _ sorter ->
                    if name == selectedColumn then
                        Just sorter

                    else
                        findSorter selectedColumn remainingColumnData

                DateColumn name _ sorter ->
                    if name == selectedColumn then
                        Just sorter

                    else
                        findSorter selectedColumn remainingColumnData

                HrefColumn name _ _ sorter ->
                    if name == selectedColumn then
                        Just sorter

                    else
                        findSorter selectedColumn remainingColumnData

                HrefColumnExtra _ _ ->
                    Nothing

                CheckColumn name _ sorter ->
                    if name == selectedColumn then
                        Just sorter

                    else
                        findSorter selectedColumn remainingColumnData

                DropdownColumn _ ->
                    Nothing

                HtmlColumn name _ sorter ->
                    if name == selectedColumn then
                        Just sorter

                    else
                        findSorter selectedColumn remainingColumnData


increasingOrDecreasingBy : ({ data | id : Int } -> comparable) -> Sorter data
increasingOrDecreasingBy toComparable =
    IncOrDec (List.sortBy toComparable)


defaultSort : ({ data | id : Int } -> Maybe String) -> Sorter data
defaultSort t =
    increasingOrDecreasingBy (Functions.defaultString << t)


defaultIntSort : ({ data | id : Int } -> Maybe Int) -> Sorter data
defaultIntSort t =
    increasingOrDecreasingBy (Functions.defaultIntToString << t)


boolToInt : Bool -> Int



--TODO, test if this sorts the same way the other columns do


boolToInt bool =
    if bool then
        0

    else
        1


defaultBoolSort : ({ data | id : Int } -> Bool) -> Sorter data
defaultBoolSort t =
    increasingOrDecreasingBy (boolToInt << t)
