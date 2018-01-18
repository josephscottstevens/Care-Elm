module Table exposing (..)

import Html exposing (Html, Attribute, div, table, th, td, tr, thead, tbody, text, button, ul, li, a, span)
import Html.Attributes exposing (class, id, style, type_, target, colspan, classList)
import Html.Events as Events
import Json.Decode as Decode
import Utils.CommonFunctions exposing (defaultDateTime, defaultString)


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
    , isReversed = False
    , sortedColumnName = sortedColumnName
    , openDropdownId = Nothing
    }


type Column data msg
    = StringColumn String ({ data | id : Int } -> String) (Sorter { data | id : Int })
    | NullableStringColumn String ({ data | id : Int } -> Maybe String) (Sorter { data | id : Int })
    | NullableDateTimeColumn String ({ data | id : Int } -> Maybe String) (Sorter { data | id : Int })
    | DropdownColumn (List ( String, String, Html.Attribute msg ))


type alias Config data msg =
    { domTableId : String
    , headers : List String
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
                        (List.map (viewTh state config) config.headers)
                    ]
                , tbody []
                    (viewTr state sortedRows config maybeCustomRow)
                ]
            , pagingView 0 (List.length sortedRows)
            ]


onCustomClick : (String -> msg) -> Attribute msg
onCustomClick tagger =
    Events.on "click"
        (Decode.map tagger (Decode.at [ "target", "id" ] Decode.string))


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

        rowClass row ctr =
            classList
                [ ( "e-row", ctr % 2 == 0 )
                , ( "e-alt_row", ctr % 2 == 1 )
                ]

        clickEvent row =
            onCustomClick
                (\t ->
                    if t == "contextMenuButton" then
                        config.toMsg { state | openDropdownId = Just row.id }
                    else
                        config.toMsg { state | selectedId = Just row.id }
                )

        standardTr ctr row =
            tr
                [ rowClass row ctr
                , selectedStyle row
                , clickEvent row
                ]
                (List.map (viewTd state row config) config.columns)

        inlineStyle =
            style
                [ ( "background-color", "white" )
                , ( "padding-top", "5px" )
                , ( "margin-left", "5px" )
                ]
    in
        if List.length rows == 0 then
            [ tr []
                [ td [] [ text "No records to display" ]
                ]
            ]
        else
            case maybeCustomRow of
                Just customRow ->
                    (tr []
                        [ td [ colspan 4, inlineStyle ]
                            [ customRow
                            ]
                        ]
                    )
                        :: List.indexedMap standardTr rows

                Nothing ->
                    List.indexedMap standardTr rows


emptyAttr : Attribute msg
emptyAttr =
    class ""


viewTh : State -> Config { data | id : Int } msg -> String -> Html msg
viewTh state config name =
    let
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
    in
        td [ tdClass, tdStyle ]
            [ case column of
                StringColumn name dataToString _ ->
                    text (dataToString row)

                NullableStringColumn name dataToString _ ->
                    text (Maybe.withDefault "" (dataToString row))

                NullableDateTimeColumn name dataToString _ ->
                    text (defaultDateTime (dataToString row))

                DropdownColumn dropDownItems ->
                    rowDropDownDiv state config.toMsg row dropDownItems
            ]



-- Custom


rowDropDownDiv : State -> (State -> msg) -> { data | id : Int } -> List ( String, String, Html.Attribute msg ) -> Html msg
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

        blurEvent =
            Events.onBlur (toMsg { state | openDropdownId = Nothing })
    in
        div []
            [ div [ style [ ( "text-align", "right" ) ] ]
                [ button [ id "contextMenuButton", type_ "button", btnClass, blurEvent, btnStyle ]
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
            (totalVisiblePages // itemsPerPage)

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
                |> List.drop ((currentPage // pagesPerBlock) * pagesPerBlock)
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

        employersCount =
            toString totalVisiblePages

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


sort : State -> List (Column data msg) -> List { data | id : Int } -> List { data | id : Int }
sort state columnData data =
    case findSorter state.sortedColumnName columnData of
        Nothing ->
            data

        Just sorter ->
            applySorter state.isReversed sorter data


applySorter : Bool -> Sorter data -> List { data | id : Int } -> List { data | id : Int }
applySorter isReversed sorter data =
    case sorter of
        None ->
            data

        IncOrDec sort ->
            if isReversed then
                List.reverse (sort data)
            else
                sort data


findSorter : String -> List (Column data msg) -> Maybe (Sorter { data | id : Int })
findSorter selectedColumn columnData =
    case columnData of
        [] ->
            Nothing

        column :: remainingColumnData ->
            case column of
                StringColumn name dataToString sorter ->
                    if name == selectedColumn then
                        Just sorter
                    else
                        findSorter selectedColumn remainingColumnData

                NullableStringColumn name dataToString sorter ->
                    if name == selectedColumn then
                        Just sorter
                    else
                        findSorter selectedColumn remainingColumnData

                NullableDateTimeColumn name dataToString sorter ->
                    if name == selectedColumn then
                        Just sorter
                    else
                        findSorter selectedColumn remainingColumnData

                DropdownColumn dropDownItems ->
                    Nothing


unsortable : Sorter { data | id : Int }
unsortable =
    None


increasingOrDecreasingBy : ({ data | id : Int } -> comparable) -> Sorter data
increasingOrDecreasingBy toComparable =
    IncOrDec (List.sortBy toComparable)


defaultSort : ({ data | id : Int } -> Maybe String) -> Sorter data
defaultSort t =
    increasingOrDecreasingBy (defaultString << t)
