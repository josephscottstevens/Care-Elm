module Common.ServerTable
    exposing
        ( State
        , Column
        , Config
        , GridOperations
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
        , encodeGridStuff
        , defaultGridOperations
        , getGridOperations
        )

import Html exposing (Html, Attribute, div, table, th, td, tr, thead, tbody, text, button, ul, li, a, span, input)
import Html.Attributes exposing (class, id, style, type_, target, colspan, classList, href, disabled, checked)
import Html.Events as Events
import Common.Functions as Functions exposing (maybeVal)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


-- Data Types


type alias State =
    { selectedId : Maybe Int
    , openDropdownId : Maybe Int
    , pageIndex : Int
    , rowsPerPage : Int
    , pagesPerBlock : Int
    , totalRows : Int
    , sortField : Maybe String
    , sortAscending : Bool
    }


type alias GridOperations =
    { pageIndex : Int
    , rowsPerPage : Int
    , pagesPerBlock : Int
    , totalRows : Int
    , sortField : Maybe String
    , sortAscending : Bool
    }


getGridOperations : State -> GridOperations
getGridOperations state =
    { pageIndex = state.pageIndex
    , rowsPerPage = state.rowsPerPage
    , pagesPerBlock = state.pagesPerBlock
    , totalRows = state.totalRows
    , sortField = state.sortField
    , sortAscending = state.sortAscending
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
    , pagesPerBlock = 15
    , totalRows = -1
    , sortField = Just "DoB"
    , sortAscending = False
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
    , toMsg : State -> msg
    , columns : List (Column { data | id : Int } msg)
    }


type Sorter data
    = None
    | IncOrDec (List { data | id : Int } -> List { data | id : Int })



-- VIEW


view : State -> List { data | id : Int } -> Config { data | id : Int } msg -> Maybe (Html msg) -> Html msg
view state rows config maybeCustomRow =
    div [ class "e-grid e-js e-waitingpopup" ]
        [ viewToolbar config.toolbar
        , table [ id config.domTableId, class "e-table", style [ ( "border-collapse", "collapse" ) ] ]
            [ thead [ class "e-gridheader e-columnheader e-hidelines" ]
                [ tr [] (List.map (viewTh state config) config.columns)
                , tr [] (List.map (viewThFilter state config) config.columns)
                ]
            , tbody []
                (viewTr state rows config maybeCustomRow)
            ]
        , pagingView state config.toMsg
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
            case state.sortField of
                Just t ->
                    if t == name then
                        if state.sortAscending then
                            [ text name, span [ class "e-icon e-ascending e-rarrowup-2x" ] [] ]
                        else
                            [ text name, span [ class "e-icon e-ascending e-rarrowdown-2x" ] [] ]
                    else
                        [ text name ]

                Nothing ->
                    [ text name ]

        newSortDirection =
            case state.sortField of
                Just t ->
                    (not state.sortAscending)

                Nothing ->
                    state.sortAscending

        sortClick =
            Events.onClick (config.toMsg { state | sortAscending = newSortDirection, sortField = Just name })
    in
        th [ class ("e-headercell e-default " ++ name), sortClick ]
            [ div [ class "e-headercelldiv e-gridtooltip" ] headerContent
            ]


viewThFilter : State -> Config { data | id : Int } msg -> Column { data | id : Int } msg -> Html msg
viewThFilter state config column =
    th [ class ("e-filterbarcell") ]
        [ div [ class "e-filterdiv e-fltrinputdiv" ]
            [ input [ class "e-ejinputtext e-filtertext" ] []
            , span [ class "e-cancel e-icon" ] []
            ]
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
                    rowDropDownDiv state config.toMsg row dropDownItems
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
        a [ class iconClass, Events.onClick event, iconStyle ] []



-- paging


setPagingState : State -> (State -> msg) -> Page -> Html.Attribute msg
setPagingState state toMsg page =
    let
        newIndex =
            case page of
                First ->
                    0

                Previous ->
                    if state.pageIndex > 0 then
                        state.pageIndex - 1
                    else
                        0

                PreviousBlock ->
                    0

                Index t ->
                    t

                NextBlock ->
                    0

                Next ->
                    state.pageIndex + 1

                Last ->
                    (state.totalRows // state.rowsPerPage) - 1
    in
        Events.onClick (toMsg { state | pageIndex = newIndex })


pagingView : State -> (State -> msg) -> Html msg
pagingView state toMsg =
    let
        totalPages =
            (state.totalRows // state.rowsPerPage) - 1

        pagingStateClick page =
            setPagingState state toMsg page

        activeOrNot pageIndex =
            let
                activeOrNotText =
                    if pageIndex == state.pageIndex then
                        "e-currentitem e-active"
                    else
                        "e-default"
            in
                div
                    [ class ("e-link e-numericitem e-spacing " ++ activeOrNotText), pagingStateClick (Index pageIndex) ]
                    [ text (toString (pageIndex + 1)) ]

        rng =
            List.range 0 totalPages
                |> List.drop ((state.pageIndex // state.pagesPerBlock) * state.pagesPerBlock)
                |> List.take state.pagesPerBlock
                |> List.map activeOrNot

        firstPageClass =
            if state.pageIndex >= state.rowsPerPage then
                "e-icon e-mediaback e-firstpage e-default"
            else
                "e-icon e-mediaback e-firstpagedisabled e-disable"

        leftPageClass =
            if state.pageIndex > 0 then
                "e-icon e-arrowheadleft-2x e-prevpage e-default"
            else
                "e-icon e-arrowheadleft-2x e-prevpagedisabled e-disable"

        leftPageBlockClass =
            if state.pageIndex >= state.pagesPerBlock then
                "e-link e-spacing e-PP e-numericitem e-default"
            else
                "e-link e-nextprevitemdisabled e-disable e-spacing e-PP"

        rightPageBlockClass =
            if state.pageIndex < totalPages - state.pagesPerBlock then
                "e-link e-NP e-spacing e-numericitem e-default"
            else
                "e-link e-NP e-spacing e-nextprevitemdisabled e-disable"

        rightPageClass =
            if state.pageIndex < totalPages then
                "e-nextpage e-icon e-arrowheadright-2x e-default"
            else
                "e-icon e-arrowheadright-2x e-nextpagedisabled e-disable"

        lastPageClass =
            if state.pageIndex < totalPages - state.pagesPerBlock then
                "e-lastpage e-icon e-mediaforward e-default"
            else
                "e-icon e-mediaforward e-animate e-lastpagedisabled e-disable"

        pagerText =
            let
                currentPageText =
                    toString (state.pageIndex + 1)

                totalPagesText =
                    toString (totalPages + 1)

                totalItemsText =
                    toString state.totalRows
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


defaultGridOperations : GridOperations
defaultGridOperations =
    { pageIndex = 0
    , rowsPerPage = 12
    , pagesPerBlock = 15
    , totalRows = -1
    , sortField = Nothing
    , sortAscending = False
    }


encodeGridOperations : GridOperations -> Encode.Value
encodeGridOperations gridOperations =
    Encode.object
        [ ( "Skip", Encode.int gridOperations.pageIndex )
        , ( "RowsPerPage", Encode.int gridOperations.rowsPerPage )
        , ( "PageSize", Encode.int gridOperations.pagesPerBlock )
        , ( "TotalRows", Encode.int gridOperations.totalRows )
        , ( "SortField", maybeVal Encode.string gridOperations.sortField )
        , ( "SortAscending", Encode.bool gridOperations.sortAscending )
        ]


getFilter : Column { data | id : Int } msg -> ( String, Encode.Value )
getFilter column =
    case column of
        IntColumn _ dataToInt _ filterField ->
            -- text (Functions.defaultIntToString (dataToInt row))
            ( filterField, Encode.string "" )

        StringColumn _ dataToString _ filterField ->
            -- text (Maybe.withDefault "" (dataToString row))
            ( filterField, Encode.string "" )

        DateTimeColumn _ dataToString _ filterField ->
            -- text (Functions.defaultDateTime (dataToString row))
            ( filterField, Encode.string "" )

        DateColumn _ dataToString _ filterField ->
            -- text (Functions.defaultDate (dataToString row))
            ( filterField, Encode.string "" )

        HrefColumn _ displayText dataToString _ filterField ->
            -- a [ href (Maybe.withDefault "" (dataToString row)), target "_blank" ] [ text displayText ]
            ( filterField, Encode.string "" )

        HrefColumnExtra _ toNode ->
            ( "", Encode.string "" )

        CheckColumn _ dataToString _ filterField ->
            ( "", Encode.string "" )

        DropdownColumn dropDownItems ->
            ( "", Encode.string "" )


encodeGridStuff : List (Column { data | id : Int } msg) -> GridOperations -> Encode.Value
encodeGridStuff columns gridOperations =
    Encode.object
        (( "GridOperations", encodeGridOperations gridOperations )
            :: [ ( "Facility", Encode.string "" )
               , ( "BillingDate", Encode.string "Sun, 04 Feb 2018 14:30:02 GMT" )
               , ( "MainProvider", Encode.string "" )
               , ( "PatientName", Encode.string "" )
               , ( "DoB", Encode.string "Sun, 04 Feb 2018 14:30:02 GMT" )
               , ( "PatientFacilityIdNo", Encode.string "" )
               , ( "AssignedTo", Encode.string "" )
               ]
        )


decodeGridOperations : Decode.Decoder GridOperations
decodeGridOperations =
    Pipeline.decode GridOperations
        |> Pipeline.required "Skip" Decode.int
        |> Pipeline.required "RowsPerPage" Decode.int
        |> Pipeline.required "PageSize" Decode.int
        |> Pipeline.required "TotalRows" Decode.int
        |> Pipeline.required "SortField" (Decode.maybe Decode.string)
        |> Pipeline.required "SortAscending" Decode.bool
