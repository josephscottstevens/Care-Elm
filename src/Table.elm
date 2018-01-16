module Table
    exposing
        ( view
        , config
        , stringColumn
        , intColumn
        , floatColumn
        , State
        , initialSort
        , Column
        , customColumn
        , veryCustomColumn
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
        , Status(..)
        , defaultCustomizations
        )

import Html exposing (Html, Attribute)
import Html.Attributes as Attr
import Html.Events as E
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy3)
import Json.Decode as Json


-- Data Types


type alias State =
    { columnName : String
    , isReversed : Bool
    }


type Status
    = Unsortable
    | Sortable Bool
    | Reversible (Maybe Bool)


type Column data msg
    = Column (ColumnData data msg)


type alias ColumnData data msg =
    { name : String
    , viewData : data -> HtmlDetails msg
    , sorter : Sorter data
    }


type Config data msg
    = Config
        { toId : data -> String
        , toMsg : State -> msg
        , columns : List (ColumnData data msg)
        , customizations : Customizations data msg
        }


type Sorter data
    = None
    | Increasing (List data -> List data)
    | Decreasing (List data -> List data)
    | IncOrDec (List data -> List data)
    | DecOrInc (List data -> List data)


type alias Customizations data msg =
    { tableAttrs : List (Attribute msg)
    , caption : Maybe (HtmlDetails msg)
    , thead : List ( String, Status, Attribute msg ) -> HtmlDetails msg
    , tfoot : Maybe (HtmlDetails msg)
    , tbodyAttrs : List (Attribute msg)
    , rowAttrs : data -> List (Attribute msg)
    }


type alias HtmlDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


initialSort : String -> State
initialSort header =
    State header False



-- CONFIG


config :
    { toId : data -> String
    , toMsg : State -> msg
    , columns : List (Column data msg)
    }
    -> Config data msg
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


defaultCustomizations : Customizations data msg
defaultCustomizations =
    { tableAttrs = []
    , caption = Nothing
    , thead = simpleThead
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



-- COLUMNS


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


view : Config data msg -> State -> List data -> Html msg
view (Config { toId, toMsg, columns, customizations }) state data =
    let
        sortedData =
            sort state columns data

        theadDetails =
            customizations.thead (List.map (toHeaderInfo state toMsg) columns)

        thead =
            Html.thead theadDetails.attributes theadDetails.children

        tbody =
            Keyed.node "tbody" customizations.tbodyAttrs <|
                List.map (viewRow toId columns customizations.rowAttrs) sortedData

        withFoot =
            case customizations.tfoot of
                Nothing ->
                    tbody :: []

                Just { attributes, children } ->
                    Html.tfoot attributes children :: tbody :: []
    in
        Html.table customizations.tableAttrs <|
            case customizations.caption of
                Nothing ->
                    thead :: withFoot

                Just { attributes, children } ->
                    Html.caption attributes children :: thead :: withFoot


toHeaderInfo : State -> (State -> msg) -> ColumnData data msg -> ( String, Status, Attribute msg )
toHeaderInfo { columnName, isReversed } toMsg { name, sorter } =
    case sorter of
        None ->
            ( name, Unsortable, onClick columnName isReversed toMsg )

        Increasing _ ->
            ( name, Sortable (name == columnName), onClick name False toMsg )

        Decreasing _ ->
            ( name, Sortable (name == columnName), onClick name False toMsg )

        IncOrDec _ ->
            if name == columnName then
                ( name, Reversible (Just isReversed), onClick name (not isReversed) toMsg )
            else
                ( name, Reversible Nothing, onClick name False toMsg )

        DecOrInc _ ->
            if name == columnName then
                ( name, Reversible (Just isReversed), onClick name (not isReversed) toMsg )
            else
                ( name, Reversible Nothing, onClick name False toMsg )


onClick : String -> Bool -> (State -> msg) -> Attribute msg
onClick name isReversed toMsg =
    E.on "click" <|
        Json.map toMsg <|
            Json.map2 State (Json.succeed name) (Json.succeed isReversed)


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
sort state columnData data =
    case findSorter state.columnName columnData of
        Nothing ->
            data

        Just sorter ->
            applySorter state.isReversed sorter data


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
