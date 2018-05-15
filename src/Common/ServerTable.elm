port module Common.ServerTable
    exposing
        ( Column
        , ColumnStyle(..)
        , Config
        , Filter
        , FilterControl(..)
        , IdAttrType(..)
        , Operator(..)
        , ServerData
        , State
        , checkColumn
        , dateColumn
        , dateTimeColumn
        , decodeGridOperations
        , defaultRowsPerPage
        , encodeGridOperations
        , hrefColumn
        , htmlColumn
        , init
        , initFilter
        , intColumn
        , stringColumn
        , textHtml
        , toolbarButton
        , updateFilter
        , updateFilters
        , updateFromServer
        , view
        )

import Common.Dates as Functions
import Common.Functions as Functions
import Html exposing (Attribute, Html, a, button, div, input, li, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (attribute, checked, class, classList, colspan, disabled, href, id, style, target, type_)
import Html.Events as Events
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Time exposing (Posix)


port initFilters : List Filter -> Cmd msg


port updateFilters : (List Filter -> msg) -> Sub msg


type Operator
    = NoOperator
    | Equals String
    | Contains String
    | Between String String
    | CustomSingleOperator String (List String)


type FilterControl
    = TextControl
    | DateControl
    | DateTimeControl
    | NoControl
    | CheckBoxControl
    | FilterIsNewControl
    | Last60MonthsControl
    | SixCirclesControl


type alias Filter =
    { controlType : String
    , columnId : String
    , names : List String
    , values : List String
    , expressions : List String
    }


type ColumnStyle
    = NoStyle
    | Width Int
    | CustomStyle (List ( String, String ))


type IdAttrType
    = IdAttr String


toIdAttr : IdAttrType -> String
toIdAttr t =
    case t of
        IdAttr str ->
            Functions.idAttr str


type alias Column data msg =
    { headerText : String
    , viewData : data -> Html msg
    , columnStyle : ColumnStyle
    , filterControl : FilterControl
    , operator : Operator
    , idAttr : IdAttrType
    }


stringColumn : String -> (data -> Maybe String) -> ColumnStyle -> String -> Column data msg
stringColumn headerText data columnStyle dataField =
    { headerText = headerText
    , viewData = data >> (\t -> text (Maybe.withDefault "" t))
    , columnStyle = columnStyle
    , filterControl = TextControl
    , operator = Contains dataField
    , idAttr = IdAttr headerText
    }


intColumn : String -> (data -> Maybe Int) -> ColumnStyle -> String -> Column data msg
intColumn headerText data columnStyle dataField =
    { headerText = headerText
    , viewData = data >> (\t -> text (Functions.defaultIntToString t))
    , columnStyle = columnStyle
    , filterControl = TextControl
    , operator = Equals dataField
    , idAttr = IdAttr headerText
    }


dateColumn : String -> (data -> Maybe Posix) -> ColumnStyle -> String -> Column data msg
dateColumn headerText data columnStyle dataField =
    { headerText = headerText
    , viewData =
        data
            >> (\t ->
                    t
                        |> Maybe.map Functions.dateToString
                        |> Maybe.withDefault ""
                        |> text
               )
    , columnStyle = columnStyle
    , filterControl = DateControl
    , operator = Equals dataField
    , idAttr = IdAttr headerText
    }


dateTimeColumn : String -> (data -> Maybe Posix) -> ColumnStyle -> String -> Column data msg
dateTimeColumn headerText data columnStyle dataField =
    { headerText = headerText
    , viewData =
        data
            >> (\t ->
                    t
                        |> Maybe.map Functions.dateTimeToString
                        |> Maybe.withDefault ""
                        |> text
               )
    , columnStyle = columnStyle
    , filterControl = DateTimeControl
    , operator = Equals dataField
    , idAttr = IdAttr headerText
    }


hrefColumn : String -> (data -> ( Maybe String, String )) -> ColumnStyle -> String -> Column data msg
hrefColumn headerText data columnStyle dataField =
    { headerText = headerText
    , viewData = data >> viewHrefColumn
    , columnStyle = columnStyle
    , filterControl = TextControl
    , operator = Contains dataField
    , idAttr = IdAttr headerText
    }


viewHrefColumn : ( Maybe String, String ) -> Html msg
viewHrefColumn ( urlData, textData ) =
    a [ href (Functions.defaultString urlData), target "_blank" ]
        [ text textData ]


checkColumn : String -> (data -> Bool) -> ColumnStyle -> String -> Column data msg
checkColumn headerText data columnStyle dataField =
    { headerText = headerText
    , viewData = data >> viewCheckColumn
    , columnStyle = columnStyle
    , filterControl = CheckBoxControl
    , operator = Equals dataField
    , idAttr = IdAttr headerText
    }


viewCheckColumn : Bool -> Html msg
viewCheckColumn isChecked =
    div [ class "e-checkcell" ]
        [ div [ class "e-checkcelldiv", style "text-align" "center" ]
            [ input [ type_ "checkbox", disabled True, checked isChecked ] []
            ]
        ]


htmlColumn : String -> (data -> Html msg) -> ColumnStyle -> FilterControl -> Operator -> IdAttrType -> Column data msg
htmlColumn headerText data columnStyle filterControl operator idAttr =
    { headerText = headerText
    , viewData = data
    , columnStyle = columnStyle
    , filterControl = filterControl
    , operator = operator
    , idAttr = idAttr
    }



-- Data Types


type alias State =
    { selectedId : Maybe Int
    , openDropdownId : Maybe Int
    , skip : Int
    , totalRows : Int
    , sortField : Maybe String
    , sortAscending : Bool
    , filters : List Filter
    , rowsPerPage : Int
    }


type alias Config data msg =
    { domTableId : String
    , rowDropdownItems : List ( String, String, data -> msg )
    , toolbar : List (Html msg)
    , columns : List (Column data msg)
    , toMsg : State -> msg
    }


init : Int -> Maybe String -> List (Column data msg) -> State
init rowsPerPage sortField columns =
    { selectedId = Nothing
    , openDropdownId = Nothing
    , skip = 0
    , totalRows = 0
    , sortField = sortField
    , sortAscending = False
    , filters = buildFilter columns
    , rowsPerPage = rowsPerPage
    }


type alias ServerData =
    { skip : Int
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


defaultRowsPerPage : Int
defaultRowsPerPage =
    10


pageFooterBlockSize : Int
pageFooterBlockSize =
    15



-- VIEW


view : State -> Config data msg -> List data -> Maybe (Html msg) -> Html msg
view gridOperations config rows maybeCustomRow =
    div [ class "e-grid e-js e-waitingpopup" ]
        [ viewToolbar config.toolbar
        , table [ id config.domTableId, class "e-table", style "border-collapse" "collapse" ]
            [ thead [ class "e-gridheader e-columnheader e-hidelines" ]
                [ tr [] (List.map (viewTh gridOperations config) config.columns ++ [ viewDropdownTh ])
                , tr [] (List.map viewThFilter config.columns)
                ]
            , tbody []
                (viewTr gridOperations config rows maybeCustomRow)
            ]
        , pagingView gridOperations config
        ]


viewTr : State -> Config data msg -> List data -> Maybe (Html msg) -> List (Html msg)
viewTr gridOperations config rows maybeCustomRow =
    let
        selectedStyle idx =
            if Just idx == gridOperations.selectedId then
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
                    ++ selectedStyle ctr
                )
                (List.map (viewTd ctr gridOperations config row) config.columns ++ [ rowDropDownDiv ctr gridOperations config row ])

        customRowStyle =
            if List.length rows == 0 then
                [ style "" "" ]

            else
                [ style "border-bottom-color" "#cecece"
                , style "border-bottom-width" "1px"
                , style "border-bottom-style" "solid"
                ]
    in
    case maybeCustomRow of
        Just customRow ->
            tr customRowStyle
                [ td
                    [ colspan (List.length config.columns)
                    , style "background-color" "white"
                    , style "padding-top" "10px"
                    , style "margin-left" "5px"
                    ]
                    [ customRow ]
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


tdClass : Bool -> Attribute msg
tdClass isActive =
    classList
        [ ( "e-gridtooltip", True )
        , ( "e-active", isActive )
        ]


tdStyle : Attribute msg
tdStyle =
    style "padding-left" "8.4px"


viewTd : Int -> State -> Config data msg -> data -> Column data msg -> Html msg
viewTd idx gridOperations config row column =
    let
        isActive =
            Just idx == gridOperations.selectedId
    in
    td
        [ tdClass isActive
        , tdStyle
        , Events.onClick (config.toMsg { gridOperations | selectedId = Just idx })
        ]
        [ column.viewData row ]


viewTh : State -> Config data msg -> Column data msg -> Html msg
viewTh gridOperations config column =
    let
        name =
            getServerField column.operator

        headerContent =
            case gridOperations.sortField of
                Just t ->
                    if Just t == name then
                        if gridOperations.sortAscending then
                            [ text column.headerText, span [ class "e-icon e-ascending e-rarrowup-2x" ] [] ]

                        else
                            [ text column.headerText, span [ class "e-icon e-ascending e-rarrowdown-2x" ] [] ]

                    else
                        [ text column.headerText ]

                Nothing ->
                    [ text column.headerText ]

        newSortDirection =
            case gridOperations.sortField of
                Just _ ->
                    not gridOperations.sortAscending

                Nothing ->
                    gridOperations.sortAscending

        sortClick =
            case name of
                Just _ ->
                    Events.onClick (config.toMsg { gridOperations | sortAscending = newSortDirection, sortField = name })

                Nothing ->
                    attribute "onclick" ""
    in
    th ([ class ("e-headercell e-default " ++ Functions.defaultString name) ] ++ getColumnStyle column.columnStyle)
        [ div [ class "e-headercelldiv e-gridtooltip", sortClick ] headerContent
        ]


viewDropdownTh : Html msg
viewDropdownTh =
    th [ class "e-headercell e-default dropdownColumn", style "width" "14px" ]
        [ div [ class "e-headercelldiv e-gridtooltip" ] []
        ]


viewThFilter : Column data msg -> Html msg
viewThFilter column =
    th [ class "e-filterbarcell e-fltrtemp" ]
        [ div [ class "e-filterdiv e-fltrtempdiv" ]
            [ input [ id (toIdAttr column.idAttr) ] []
            ]
        ]


textHtml : String -> Html msg
textHtml t =
    div
        [ Encode.string t
            |> Html.Attributes.property "innerHTML"
        ]
        []


getColumnStyle : ColumnStyle -> List (Attribute msg1)
getColumnStyle columnStyle =
    case columnStyle of
        NoStyle ->
            [ style "" "" ]

        Width int ->
            [ style "width" (String.fromInt int ++ "%") ]

        CustomStyle list ->
            list |> List.map (\( t, y ) -> style t y)


getServerField : Operator -> Maybe String
getServerField operator =
    case operator of
        NoOperator ->
            Nothing

        Equals serverFieldName ->
            Just serverFieldName

        Contains serverFieldName ->
            Just serverFieldName

        Between serverFieldName _ ->
            Just serverFieldName

        CustomSingleOperator op items ->
            Nothing



-- Custom


rowDropDownDiv : Int -> State -> Config data msg -> data -> Html msg
rowDropDownDiv idx gridOperations config row =
    let
        dropClickEvent event =
            Events.onClick (event row)

        dropDownMenuItem : ( String, String, data -> msg ) -> Html msg
        dropDownMenuItem ( iconClass, displayText, event ) =
            li [ class "e-content e-list", dropClickEvent event ]
                [ a [ class "e-menulink", target "_blank" ]
                    [ text displayText
                    , span [ class ("e-gridcontext e-icon " ++ iconClass) ] []
                    ]
                ]

        dropDownMenuStyle : List (Attribute msg)
        dropDownMenuStyle =
            [ style "z-index" "5000"
            , style "position" "absolute"
            , style "display" "block"
            , style "left" "-173px"
            , style "width" "178.74px"
            ]

        dropMenu =
            case gridOperations.openDropdownId of
                Just t ->
                    if idx == t then
                        [ ul [ class "e-menu e-js e-widget e-box e-separator" ]
                            (List.map dropDownMenuItem config.rowDropdownItems)
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
            case gridOperations.openDropdownId of
                Just _ ->
                    Events.onClick (config.toMsg { gridOperations | openDropdownId = Nothing })

                Nothing ->
                    Events.onClick (config.toMsg { gridOperations | openDropdownId = Just idx })

        blurEvent =
            Events.onBlur (config.toMsg { gridOperations | openDropdownId = Nothing })
    in
    div []
        [ div [ style "text-align" "right" ]
            [ button [ id "contextMenuButton", type_ "button", btnClass, clickEvent, blurEvent, btnStyle ]
                [ div ([ id "editButtonMenu" ] ++ dropDownMenuStyle)
                    dropMenu
                ]
            ]
        ]


viewToolbar : List (Html msg) -> Html msg
viewToolbar items =
    div [ class "e-gridtoolbar e-toolbar e-js e-widget e-box e-toolbarspan e-tooltip" ]
        [ ul [ class "e-ul e-horizontal" ]
            [ li [ class "e-tooltxt" ]
                items
            ]
        ]


toolbarButton : String -> msg -> Html msg
toolbarButton iconStr event =
    let
        iconStyle =
            if String.contains "e-disable" iconStr then
                style "" ""

            else
                style "cursor" "pointer"

        iconClass =
            "e-addnewitem e-toolbaricons e-icon " ++ iconStr
    in
    a [ class iconClass, Events.onClick event, iconStyle ] []



-- paging


setPagingState : State -> Config data msg -> Page -> Attribute msg
setPagingState gridOperations config page =
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
                    (gridOperations.totalRows // gridOperations.rowsPerPage) - 1
    in
    Events.onClick (config.toMsg { gridOperations | skip = newIndex })


pagingView : State -> Config data msg -> Html msg
pagingView gridOperations config =
    let
        totalPages =
            gridOperations.totalRows // gridOperations.rowsPerPage

        pagingStateClick page =
            setPagingState gridOperations config page

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
                [ text (String.fromInt (skip + 1)) ]

        rng =
            List.range 0 totalPages
                |> List.drop ((gridOperations.skip // pageFooterBlockSize) * pageFooterBlockSize)
                |> List.take pageFooterBlockSize
                |> List.map activeOrNot

        firstPageClass =
            if gridOperations.skip >= pageFooterBlockSize then
                "e-icon e-mediaback e-firstpage e-default"

            else
                "e-icon e-mediaback e-firstpagedisabled e-disable"

        leftPageClass =
            if gridOperations.skip > 0 then
                "e-icon e-arrowheadleft-2x e-prevpage e-default"

            else
                "e-icon e-arrowheadleft-2x e-prevpagedisabled e-disable"

        leftPageBlockClass =
            if gridOperations.skip >= pageFooterBlockSize then
                "e-link e-spacing e-PP e-numericitem e-default"

            else
                "e-link e-nextprevitemdisabled e-disable e-spacing e-PP"

        rightPageBlockClass =
            if gridOperations.skip < totalPages - pageFooterBlockSize then
                "e-link e-NP e-spacing e-numericitem e-default"

            else
                "e-link e-NP e-spacing e-nextprevitemdisabled e-disable"

        rightPageClass =
            if gridOperations.skip < totalPages then
                "e-nextpage e-icon e-arrowheadright-2x e-default"

            else
                "e-icon e-arrowheadright-2x e-nextpagedisabled e-disable"

        lastPageClass =
            if gridOperations.skip < totalPages - pageFooterBlockSize then
                "e-lastpage e-icon e-mediaforward e-default"

            else
                "e-icon e-mediaforward e-animate e-lastpagedisabled e-disable"

        pagerText =
            let
                currentPageText =
                    String.fromInt (gridOperations.skip + 1)

                totalPagesText =
                    String.fromInt <|
                        if totalPages < 1 then
                            1

                        else
                            totalPages + 1

                totalItemsText =
                    String.fromInt gridOperations.totalRows
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



--Server Stuff


updateFromServer : ServerData -> State -> State
updateFromServer serverData dt =
    { dt
        | skip = serverData.skip
        , totalRows = serverData.totalRows
        , sortField = serverData.sortField
        , sortAscending = serverData.sortAscending
    }


encodeFilter : Filter -> Encode.Value
encodeFilter filter =
    Encode.object
        [ ( "names", Encode.list Encode.string filter.names )
        , ( "controlType", Encode.string filter.controlType )
        , ( "values", Encode.list Encode.string filter.values )
        , ( "expressions", Encode.list Encode.string filter.expressions )
        ]


encodeGridOperations : State -> Encode.Value
encodeGridOperations gridOperations =
    Encode.object
        [ ( "Skip", Encode.int gridOperations.skip )
        , ( "RowsPerPage", Encode.int gridOperations.rowsPerPage )
        , ( "TotalRows", Encode.int gridOperations.totalRows )
        , ( "SortField", Functions.maybeVal Encode.string gridOperations.sortField )
        , ( "SortAscending", Encode.bool gridOperations.sortAscending )
        , ( "filters", Encode.list encodeFilter gridOperations.filters )
        ]


decodeGridOperations : Decode.Decoder ServerData
decodeGridOperations =
    Decode.succeed ServerData
        |> Pipeline.required "Skip" Decode.int
        |> Pipeline.required "RowsPerPage" Decode.int
        |> Pipeline.required "TotalRows" Decode.int
        |> Pipeline.required "SortField" (Decode.maybe Decode.string)
        |> Pipeline.required "SortAscending" Decode.bool



-- filter stuff


getControlString : FilterControl -> String
getControlString filterControl =
    case filterControl of
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

        SixCirclesControl ->
            "sixCirclesControl"


getNames : Operator -> List String
getNames operator =
    case operator of
        NoOperator ->
            []

        Equals str ->
            [ str ]

        Contains str ->
            [ str ]

        Between str1 str2 ->
            [ str1, str2 ]

        CustomSingleOperator _ items ->
            items


getOperators : Operator -> List String
getOperators operator =
    case operator of
        NoOperator ->
            []

        Equals _ ->
            [ "Equals" ]

        Contains _ ->
            [ "Contains" ]

        Between _ _ ->
            [ "GreaterThanOrEquals", "LessThanOrEquals" ]

        CustomSingleOperator op items ->
            List.map (\_ -> op) items


buildFilter : List (Column data msg) -> List Filter
buildFilter columns =
    columns
        |> List.map
            (\column ->
                { controlType = getControlString column.filterControl
                , columnId = toIdAttr column.idAttr
                , names = getNames column.operator
                , values = List.map (\_ -> "") (getNames column.operator)
                , expressions = getOperators column.operator
                }
            )


initFilter : List (Column data msg) -> Cmd msg
initFilter columns =
    buildFilter columns
        |> initFilters


updateFilter : List Filter -> State -> State
updateFilter filters gridOperations =
    { gridOperations | filters = filters }
