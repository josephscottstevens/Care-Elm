module Table exposing (..)

import Html exposing (Html, Attribute, div, table, th, td, tr, thead, tbody, text, button, ul, li, a, span)
import Html.Attributes exposing (class, id, style, type_, target, colspan, classList)
import Html.Events as Events
import Json.Decode as Decode


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



-- VIEW


view : State -> List (Row msg) -> Config msg -> Maybe (Html msg) -> Html msg
view state rows config maybeCustomRow =
    div [ class "e-grid e-js e-waitingpopup" ]
        [ viewToolbar config.toolbar
        , div [ class "e-gridheader e-textover e-hidelines" ]
            [ table [ id config.domTableId, class "e-table" ]
                [ thead []
                    [ tr [ class "e-columnheader" ]
                        (case List.head rows of
                            Just firstRow ->
                                List.map (viewTh state config) firstRow.columns

                            Nothing ->
                                List.map (viewSimpleTh state emptyAttr False) config.headers
                        )
                    ]
                , tbody [ class "e-hide" ] []
                ]
            ]
        , div [ class "e-gridcontent e-hidelines" ]
            [ table [ class "e-table" ]
                [ tbody [] (viewTr state rows config maybeCustomRow)
                ]
            ]
        , pagingView 0 (List.length rows)
        ]


onCustomClick : (String -> msg) -> Attribute msg
onCustomClick tagger =
    Events.on "click"
        (Decode.map tagger (Decode.at [ "target", "id" ] Decode.string))


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

        rowClass row ctr =
            classList
                [ ( "e-row", ctr % 2 == 0 )
                , ( "e-alt_row", ctr % 2 == 1 )
                ]

        clickEvent row =
            onCustomClick
                (\t ->
                    if t == "contextMenuButton" then
                        config.toMsg { state | openDropdownId = Just row.rowId }
                    else
                        config.toMsg { state | selectedId = Just row.rowId }
                )

        standardTr ctr row =
            tr
                [ rowClass row ctr
                , selectedStyle row
                , clickEvent row
                ]
                (List.map (viewTd state row config) row.columns)

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
                    :: List.indexedMap standardTr rows

            Nothing ->
                List.indexedMap standardTr rows


emptyAttr : Attribute msg
emptyAttr =
    class ""


viewTh : State -> Config msg -> Column msg -> Html msg
viewTh state config column =
    case column of
        StringColumn name data ->
            let
                thClick =
                    Events.onClick (config.toMsg { state | sortedColumnName = name, isReversed = not state.isReversed })

                isReversed =
                    state.sortedColumnName == name
            in
                viewSimpleTh state thClick isReversed name

        DropdownColumn _ ->
            viewSimpleTh state emptyAttr False "dropdown"


viewSimpleTh : State -> Attribute msg -> Bool -> String -> Html msg
viewSimpleTh state attr isReversed name =
    let
        headerContent =
            if state.sortedColumnName == name then
                if isReversed then
                    [ text name, span [ class "e-icon e-ascending e-rarrowup-2x" ] [] ]
                else
                    [ text name, span [ class "e-icon e-ascending e-rarrowdown-2x" ] [] ]
            else
                [ text name ]
    in
        th [ class ("e-headercell e-default " ++ name), attr ]
            [ div [ class "e-headercelldiv e-gridtooltip" ] headerContent
            ]


viewTd : State -> Row msg -> Config msg -> Column msg -> Html msg
viewTd state row config column =
    let
        tdClass =
            classList
                [ ( "e-gridtooltip", True )
                , ( "e-active", Just row.rowId == state.selectedId )

                --e-rowcell (Removed due to it clipping dropdown menu)
                ]

        tdStyle =
            style [ ( "padding-left", "8.4px" ) ]
    in
        td [ tdClass, tdStyle ]
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

        blurEvent =
            Events.onBlur (toMsg { state | openDropdownId = Nothing })
    in
        div []
            [ div [ style [ ( "text-align", "right" ) ] ]
                [ button [ id "contextMenuButton", type_ "button", btnClass, btnStyle ]
                    --blurEvent
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
