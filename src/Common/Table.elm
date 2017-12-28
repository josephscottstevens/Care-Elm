module Common.Table
    exposing
        ( view
        , config
        , stringColumn
        , intColumn
        , floatColumn
        , dropdownColumn
        , State
        , initialSort
        , Column
        , customColumn
        , veryCustomColumn
        , dropdownDetails
        , Sorter
        , unsortable
        , increasingBy
        , decreasingBy
        , increasingOrDecreasingBy
        , decreasingOrIncreasingBy
        , Config
        , customConfig
        , Customizations
        , HtmlDetails
        , Status(Unsortable, Sortable, Reversible)
        , defaultCustomizations
        )

import Html exposing (Html, Attribute)
import Html.Attributes as Attr
import Html.Events as E
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy3)
import Json.Decode as Json


-- STATE


type State
    = State String Bool Bool


initialSort : String -> State
initialSort header =
    State header False False



-- CONFIG


type Config data msg
    = Config
        { toId : data -> String
        , toMsg : State -> msg
        , columns : List (ColumnData data msg)
        , customizations : Customizations data msg
        }


config :
    { toId : { a | dropdownOpen : Bool, id : Int } -> String
    , toMsg : State -> msg
    , columns : List (Column { a | dropdownOpen : Bool, id : Int } msg)
    }
    -> Config { a | dropdownOpen : Bool, id : Int } msg
config { toId, toMsg, columns } =
    Config
        { toId = toId
        , toMsg = toMsg
        , columns = List.map (\(Column cData) -> cData) columns
        , customizations = defaultCustomizations
        }


customConfig :
    { toId : data -> String
    , toMsg : State -> msg
    , columns : List (Column data msg)
    , customizations : Customizations data msg
    }
    -> Config data msg
customConfig { toId, toMsg, columns, customizations } =
    Config
        { toId = toId
        , toMsg = toMsg
        , columns = List.map (\(Column cData) -> cData) columns
        , customizations = customizations
        }


type alias Customizations data msg =
    { tableAttrs : List (Attribute msg)
    , caption : Maybe (HtmlDetails msg)
    , thead : List ( String, Status, Attribute msg ) -> HtmlDetails msg
    , theadButtons : List ( String, Attribute msg )
    , dropdownItems : List ( String, String, Attribute msg )
    , tfoot : Maybe (HtmlDetails msg)
    , tbodyAttrs : List (Attribute msg)
    , rowAttrs : data -> List (Attribute msg)
    }


type alias HtmlDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


defaultCustomizations : Customizations { a | dropdownOpen : Bool, id : Int } msg
defaultCustomizations =
    { tableAttrs = []
    , caption = Nothing
    , thead = simpleThead
    , theadButtons = []
    , dropdownItems = []
    , tfoot = Nothing
    , tbodyAttrs = []
    , rowAttrs = simpleRowAttrs
    }


simpleThead : List ( String, Status, Attribute msg ) -> HtmlDetails msg
simpleThead headers =
    HtmlDetails [] (List.map simpleTheadHelp headers)


simpleTheadHelp : ( String, Status, Attribute msg ) -> Html msg
simpleTheadHelp ( name, status, onClick ) =
    let
        content =
            case status of
                Unsortable ->
                    [ Html.text name ]

                Sortable selected ->
                    [ Html.text name
                    , if selected then
                        darkGrey "↓"
                      else
                        lightGrey "↓"
                    ]

                Reversible Nothing ->
                    [ Html.text name
                    , lightGrey "↕"
                    ]

                Reversible (Just isReversed) ->
                    [ Html.text name
                    , darkGrey
                        (if isReversed then
                            "↑"
                         else
                            "↓"
                        )
                    ]
    in
        Html.th [ onClick ] content


darkGrey : String -> Html msg
darkGrey symbol =
    Html.span [ Attr.style [ ( "color", "#555" ) ] ] [ Html.text (" " ++ symbol) ]


lightGrey : String -> Html msg
lightGrey symbol =
    Html.span [ Attr.style [ ( "color", "#ccc" ) ] ] [ Html.text (" " ++ symbol) ]


simpleRowAttrs : data -> List (Attribute msg)
simpleRowAttrs _ =
    []


type Status
    = Unsortable
    | Sortable Bool
    | Reversible (Maybe Bool)



-- COLUMNS


type Column data msg
    = Column (ColumnData data msg)


type alias ColumnData data msg =
    { name : String
    , viewData : data -> HtmlDetails msg
    , sorter : Sorter data
    }


stringColumn : String -> (data -> String) -> Column data msg
stringColumn name toStr =
    Column
        { name = name
        , viewData = textDetails << toStr
        , sorter = increasingOrDecreasingBy toStr
        }


intColumn : String -> (data -> Int) -> Column data msg
intColumn name toInt =
    Column
        { name = name
        , viewData = textDetails << toString << toInt
        , sorter = increasingOrDecreasingBy toInt
        }


floatColumn : String -> (data -> Float) -> Column data msg
floatColumn name toFloat =
    Column
        { name = name
        , viewData = textDetails << toString << toFloat
        , sorter = increasingOrDecreasingBy toFloat
        }


textDetails : String -> HtmlDetails msg
textDetails str =
    HtmlDetails [] [ Html.text str ]


customColumn :
    { name : String
    , viewData : data -> String
    , sorter : Sorter data
    }
    -> Column data msg
customColumn { name, viewData, sorter } =
    Column <|
        ColumnData name (textDetails << viewData) sorter


veryCustomColumn :
    { name : String
    , viewData : data -> HtmlDetails msg
    , sorter : Sorter data
    }
    -> Column data msg
veryCustomColumn =
    Column



-- VIEW


view : Config { a | dropdownOpen : Bool, id : Int } msg -> State -> List { a | dropdownOpen : Bool, id : Int } -> Html msg
view (Config { toId, toMsg, columns, customizations }) state data =
    let
        cols =
            List.length columns

        newColumns =
            columns

        sortedData =
            sort state columns data

        theadDetails =
            customizations.thead (List.map (toHeaderInfo state toMsg) newColumns)

        thead =
            Html.thead theadDetails.attributes theadDetails.children

        aClass t =
            Attr.class ("e-addnewitem e-toolbaricons e-icon " ++ t)

        thClass =
            Attr.class "e-columnheader e-default e-filterbarcell Description-Column"

        aStyle =
            Attr.style [ ( "cursor", "pointer" ) ]

        thStyle =
            Attr.style [ ( "width", "100%" ) ]

        theadbuttonsDetails =
            customizations.theadButtons
                |> List.map (\( t, event ) -> Html.a [ aClass t, aStyle, event ] [])

        theadbuttons =
            Html.thead theadDetails.attributes [ Html.th [ thClass, thStyle, Attr.colspan cols ] theadbuttonsDetails ]

        tbody =
            Keyed.node "tbody" customizations.tbodyAttrs <|
                List.map (viewRow toId newColumns customizations.rowAttrs) sortedData

        withFoot =
            case customizations.tfoot of
                Nothing ->
                    [ tbody ]

                Just { attributes, children } ->
                    [ Html.tfoot attributes children, tbody ]
    in
        Html.table customizations.tableAttrs <|
            case customizations.caption of
                Nothing ->
                    theadbuttons :: thead :: withFoot

                Just { attributes, children } ->
                    Html.caption attributes children :: theadbuttons :: thead :: withFoot


toHeaderInfo : State -> (State -> msg) -> ColumnData { a | dropdownOpen : Bool, id : Int } msg -> ( String, Status, Attribute msg )
toHeaderInfo (State sortName isReversed dropdownState) toMsg { name, sorter } =
    case sorter of
        None ->
            ( name, Unsortable, onClick sortName isReversed dropdownState toMsg )

        Increasing _ ->
            ( name, Sortable (name == sortName), onClick name False dropdownState toMsg )

        Decreasing _ ->
            ( name, Sortable (name == sortName), onClick name False dropdownState toMsg )

        IncOrDec _ ->
            if name == sortName then
                ( name, Reversible (Just isReversed), onClick name (not isReversed) dropdownState toMsg )
            else
                ( name, Reversible Nothing, onClick name False dropdownState toMsg )

        DecOrInc _ ->
            if name == sortName then
                ( name, Reversible (Just isReversed), onClick name (not isReversed) dropdownState toMsg )
            else
                ( name, Reversible Nothing, onClick name False dropdownState toMsg )


onClick : String -> Bool -> Bool -> (State -> msg) -> Attribute msg
onClick name isReversed dropdownState toMsg =
    E.on "click" <|
        Json.map toMsg <|
            Json.map3 State (Json.succeed name) (Json.succeed isReversed) (Json.succeed dropdownState)


viewRow : (data -> String) -> List (ColumnData data msg) -> (data -> List (Attribute msg)) -> data -> ( String, Html msg )
viewRow toId columns toRowAttrs data =
    ( toId data
    , lazy3 viewRowHelp columns toRowAttrs data
    )


viewRowHelp : List (ColumnData data msg) -> (data -> List (Attribute msg)) -> data -> Html msg
viewRowHelp columns toRowAttrs data =
    Html.tr (toRowAttrs data) (List.map (viewCell data) columns)


viewCell : data -> ColumnData data msg -> Html msg
viewCell data { viewData } =
    let
        details =
            viewData data
    in
        Html.td details.attributes details.children



-- SORTING


sort : State -> List (ColumnData data msg) -> List data -> List data
sort (State selectedColumn isReversed _) columnData data =
    case findSorter selectedColumn columnData of
        Nothing ->
            data

        Just sorter ->
            applySorter isReversed sorter data


applySorter : Bool -> Sorter data -> List data -> List data
applySorter isReversed sorter data =
    case sorter of
        None ->
            data

        Increasing sort ->
            sort data

        Decreasing sort ->
            List.reverse (sort data)

        IncOrDec sort ->
            if isReversed then
                List.reverse (sort data)
            else
                sort data

        DecOrInc sort ->
            if isReversed then
                sort data
            else
                List.reverse (sort data)


findSorter : String -> List (ColumnData data msg) -> Maybe (Sorter data)
findSorter selectedColumn columnData =
    case columnData of
        [] ->
            Nothing

        { name, sorter } :: remainingColumnData ->
            if name == selectedColumn then
                Just sorter
            else
                findSorter selectedColumn remainingColumnData



-- SORTERS


type Sorter data
    = None
    | Increasing (List data -> List data)
    | Decreasing (List data -> List data)
    | IncOrDec (List data -> List data)
    | DecOrInc (List data -> List data)


unsortable : Sorter data
unsortable =
    None


increasingBy : (data -> comparable) -> Sorter data
increasingBy toComparable =
    Increasing (List.sortBy toComparable)


decreasingBy : (data -> comparable) -> Sorter data
decreasingBy toComparable =
    Decreasing (List.sortBy toComparable)


decreasingOrIncreasingBy : (data -> comparable) -> Sorter data
decreasingOrIncreasingBy toComparable =
    DecOrInc (List.sortBy toComparable)


increasingOrDecreasingBy : (data -> comparable) -> Sorter data
increasingOrDecreasingBy toComparable =
    IncOrDec (List.sortBy toComparable)



-- extra for dropdown


dropdownColumn : (data -> HtmlDetails msg) -> Column data msg
dropdownColumn t =
    veryCustomColumn
        { name = ""
        , viewData = t
        , sorter = unsortable
        }


dropdownDetails : List ( String, String, Attribute msg ) -> Bool -> State -> (State -> msg) -> HtmlDetails msg
dropdownDetails dropDownItems isOpen (State sortName isReversed dropdownState) toMsg =
    let
        btnClass =
            Attr.class "btn btn-sm btn-default fa fa-angle-down btn-context-menu editDropDown"

        btnStyle =
            Attr.style [ ( "position", "relative" ) ]
    in
        HtmlDetails []
            [ Html.div
                [ Attr.style [ ( "text-align", "right" ) ]
                , onClick sortName isReversed (not isOpen) toMsg
                ]
                [ Html.button
                    [ Attr.type_ "button"
                    , btnClass
                    , btnStyle
                    , if dropdownState == True then
                        onBlur sortName isReversed False toMsg
                      else
                        Attr.src ""
                    ]
                    [ Html.div
                        [ Attr.style
                            [ ( "z-index", "5000" )
                            , ( "position", "absolute" )
                            , ( "display", "block" )
                            , ( "left", "-173px" )
                            , ( "width", "178.74px" )
                            ]
                        ]
                        (dropMenu dropdownState dropDownItems)
                    ]
                ]
            ]


dropMenu : Bool -> List ( String, String, Attribute msg ) -> List (Html msg)
dropMenu dropdownState dropDownItems =
    case dropdownState of
        True ->
            [ Html.ul
                [ Attr.class "e-menu e-js e-widget e-box e-separator"
                ]
                (List.map dropDownMenuItem dropDownItems)
            ]

        False ->
            []


dropDownMenuItem : ( String, String, Attribute msg ) -> Html msg
dropDownMenuItem ( iconClass, displayText, menuMessage ) =
    Html.li [ Attr.class "e-content e-list" ]
        [ Html.a
            [ Attr.class "e-menulink"
            , menuMessage
            , Attr.target "_blank"
            ]
            [ Html.text displayText
            , Html.span [ Attr.class ("e-gridcontext e-icon " ++ iconClass) ] []
            ]
        ]


dropdownList : List ( String, String )
dropdownList =
    [ ( "position", "absolute" )
    , ( "top", "32px" )
    , ( "border-radius", "4px" )
    , ( "box-shadow", "0 1px 2px rgba(0,0,0,.24)" )
    , ( "padding", "0" )
    , ( "margin", "0" )

    -- , ( "width", "150px" )
    , ( "background-color", "white" )
    , ( "max-height", "152px" )
    , ( "overflow-x", "hidden" )
    , ( "overflow-y", "scroll" )
    , ( "z-index", "100" )
    ]


onBlur : String -> Bool -> Bool -> (State -> msg) -> Attribute msg
onBlur name isReversed dropdownState toMsg =
    E.on "blur" <|
        Json.map toMsg <|
            Json.map3 State (Json.succeed name) (Json.succeed isReversed) (Json.succeed dropdownState)
