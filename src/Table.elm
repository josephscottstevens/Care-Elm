module Table
    exposing
        ( State
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
        , htmlColumn
        )

import Html exposing (Html, Attribute, div, table, th, td, tr, thead, tbody, text, button, ul, li, a, span, input)
import Html.Attributes exposing (class, id, style, type_, target, colspan, classList, href, disabled, checked)
import Html.Events as Events
import Common.Functions as Functions
import Json.Encode as Encode


-- Data Types


type alias State =
    { selectedId : Maybe Int
    , isReversed : Bool
    , sortedColumnName : String
    , openDropdownId : Maybe Int
    }


init : String -> State
init sortedColumnName =
    { selectedId = Nothing
    , isReversed = True
    , sortedColumnName = sortedColumnName
    , openDropdownId = Nothing
    }


type Column data msg
    = IntColumn String ({ data | id : Int } -> Maybe Int) (Sorter data)
    | StringColumn String ({ data | id : Int } -> Maybe String) (Sorter data)
    | DateTimeColumn String ({ data | id : Int } -> Maybe String) (Sorter data)
    | DateColumn String ({ data | id : Int } -> Maybe String) (Sorter data)
    | HrefColumn String String ({ data | id : Int } -> Maybe String) (Sorter data)
    | HrefColumnExtra String ({ data | id : Int } -> Html msg)
    | CheckColumn String ({ data | id : Int } -> Bool) (Sorter data)
    | DropdownColumn (List ( String, String, Int -> msg ))
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


dropdownColumn : List ( String, String, Int -> msg ) -> Column data msg
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
            sort state config.columns rows
    in
        div [ class "e-grid e-js e-waitingpopup" ]
            [ viewToolbar config.toolbar
            , table [ id config.domTableId, class "e-table", style [ ( "border-collapse", "collapse" ) ] ]
                [ thead [ class "e-gridheader e-columnheader e-hidelines" ]
                    [ tr []
                        (List.map (viewTh state config) config.columns)
                    ]
                , tbody []
                    (viewTr state sortedRows config maybeCustomRow)
                ]
            , pagingView 0 (List.length sortedRows)
            ]


viewTr : State -> List { data | id : Int } -> Config { data | id : Int } msg -> Maybe (Html msg) -> List (Html msg)
viewTr state rows config maybeCustomRow =
    let
        selectedStyle row =
            style
                (if Just row.id == state.selectedId then
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
                (List.map (viewTd state row config) config.columns)

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


viewTh : State -> Config { data | id : Int } msg -> Column { data | id : Int } msg -> Html msg
viewTh state config column =
    let
        name =
            getColumnName column

        headerContent =
            if state.sortedColumnName == name then
                if state.isReversed then
                    [ text name, span [ class "e-icon e-ascending e-rarrowup-2x" ] [] ]
                else
                    [ text name, span [ class "e-icon e-ascending e-rarrowdown-2x" ] [] ]
            else
                [ text name ]

        sortClick =
            Events.onClick (config.toMsg { state | isReversed = not state.isReversed, sortedColumnName = name })
    in
        th [ class ("e-headercell e-default " ++ name), sortClick ]
            [ div [ class "e-headercelldiv e-gridtooltip" ] headerContent
            ]


viewTd : State -> { data | id : Int } -> Config { data | id : Int } msg -> Column { data | id : Int } msg -> Html msg
viewTd state row config column =
    let
        tdClass =
            classList
                [ ( "e-gridtooltip", True )
                , ( "e-active", Just row.id == state.selectedId )
                ]

        tdStyle =
            style [ ( "padding-left", "8.4px" ) ]

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


rowDropDownDiv : State -> (State -> msg) -> { data | id : Int } -> List ( String, String, Int -> msg ) -> Html msg
rowDropDownDiv state toMsg row dropDownItems =
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
            style [ ( "position", "relative" ) ]

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
        a [ class iconClass, Events.onClick event, iconStyle, id "btnNewRecord" ] []



-- paging


itemsPerPage : Int
itemsPerPage =
    10


pagesPerBlock : Int
pagesPerBlock =
    8


pagingView : Int -> Int -> Html msg
pagingView currentPage totalVisiblePages =
    let
        totalPages =
            totalVisiblePages // itemsPerPage

        activeOrNot pageIndex =
            let
                activeOrNotText =
                    if pageIndex == currentPage then
                        "e-currentitem e-active"
                    else
                        "e-default"
            in
                div
                    [ class ("e-link e-numericitem e-spacing " ++ activeOrNotText) ]
                    -- onClick (SetPagingState (Index pageIndex))
                    [ text (toString (pageIndex + 1)) ]

        rng =
            List.range 0 totalPages
                |> List.drop (currentPage // pagesPerBlock * pagesPerBlock)
                |> List.take pagesPerBlock
                |> List.map activeOrNot

        firstPageClass =
            if currentPage >= pagesPerBlock then
                "e-icon e-mediaback e-firstpage e-default"
            else
                "e-icon e-mediaback e-firstpagedisabled e-disable"

        leftPageClass =
            if currentPage > 0 then
                "e-icon e-arrowheadleft-2x e-prevpage e-default"
            else
                "e-icon e-arrowheadleft-2x e-prevpagedisabled e-disable"

        leftPageBlockClass =
            if currentPage >= pagesPerBlock then
                "e-link e-spacing e-PP e-numericitem e-default"
            else
                "e-link e-nextprevitemdisabled e-disable e-spacing e-PP"

        rightPageBlockClass =
            if currentPage < totalPages - pagesPerBlock then
                "e-link e-NP e-spacing e-numericitem e-default"
            else
                "e-link e-NP e-spacing e-nextprevitemdisabled e-disable"

        rightPageClass =
            if currentPage < totalPages then
                "e-nextpage e-icon e-arrowheadright-2x e-default"
            else
                "e-icon e-arrowheadright-2x e-nextpagedisabled e-disable"

        lastPageClass =
            if currentPage < totalPages - pagesPerBlock then
                "e-lastpage e-icon e-mediaforward e-default"
            else
                "e-icon e-mediaforward e-animate e-lastpagedisabled e-disable"

        pagerText =
            let
                currentPageText =
                    toString (currentPage + 1)

                totalPagesText =
                    toString (totalPages + 1)

                totalItemsText =
                    toString totalVisiblePages
            in
                currentPageText ++ " of " ++ totalPagesText ++ " pages (" ++ totalItemsText ++ " items)"
    in
        div [ class "e-pager e-js e-pager" ]
            [ div [ class "e-pagercontainer" ]
                [ div [ class firstPageClass ] [] --, onClick (SetPagingState First) ] []
                , div [ class leftPageClass ] [] --, onClick (SetPagingState Previous) ] []
                , a [ class leftPageBlockClass ] [] --, onClick (SetPagingState PreviousBlock) ] [ text "..." ]
                , div [ class "e-numericcontainer e-default" ] rng
                , a [ class rightPageBlockClass ] [] --, onClick (SetPagingState NextBlock) ] [ text "..." ]
                , div [ class rightPageClass ] [] --, onClick (SetPagingState Next) ] []
                , div [ class lastPageClass ] [] --, onClick (SetPagingState Last) ] []
                ]
            , div [ class "e-parentmsgbar", style [ ( "text-align", "right" ) ] ]
                [ span [ class "e-pagermsg" ] [ text pagerText ]
                ]
            ]



-- Sorting


sort : State -> List (Column { data | id : Int } msg) -> List { data | id : Int } -> List { data | id : Int }
sort state columnData data =
    case findSorter state.sortedColumnName columnData of
        Nothing ->
            data

        Just sorter ->
            applySorter state.isReversed sorter data


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


defaultBoolSort : ({ data | id : Int } -> Bool) -> Sorter data
defaultBoolSort t =
    increasingOrDecreasingBy (toString << t)
