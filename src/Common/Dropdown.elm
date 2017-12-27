port module Common.Dropdown exposing (Dropdown, Msg, init, update, view, viewGrid)

import Html exposing (Html, Attribute, div, span, text, li, ul, input, button, a)
import Html.Attributes exposing (style, value, class, readonly, type_, target)
import Html.Events as Events
import Json.Decode
import Common.Types exposing (DropdownItem, MenuMessage)
import Common.Functions as Functions
import Char
import Array exposing (Array)


port dropdownMenuScroll : String -> Cmd msg


port sendMenuMessageNew : MenuMessage -> Cmd msg


scrollToDomId : String -> Cmd msg
scrollToDomId =
    dropdownMenuScroll


type alias Dropdown =
    { isOpen : Bool
    , selectedItem : DropdownItem
    , highlightedItem : Maybe DropdownItem
    , highlightedIndex : Int
    , dropdownSource : Array DropdownItem
    , searchString : String
    , id : String
    }


emptyItem : DropdownItem
emptyItem =
    DropdownItem Nothing ""


defaultSelectedItem : Maybe DropdownItem -> DropdownItem
defaultSelectedItem selectedItem =
    Maybe.withDefault emptyItem selectedItem


init : String -> List DropdownItem -> Maybe DropdownItem -> Dropdown
init id list selectedItem =
    { isOpen = False
    , selectedItem = defaultSelectedItem selectedItem
    , highlightedItem = Nothing
    , highlightedIndex = 0
    , dropdownSource = Array.fromList list
    , searchString = ""
    , id = id
    }


type Key
    = Esc
    | Enter
    | ArrowUp
    | ArrowDown
    | PageUp
    | PageDown
    | Home
    | End
    | Searchable Char


type Msg
    = ItemPicked DropdownItem
    | ItemEntered DropdownItem
    | ItemLeft DropdownItem
    | SetOpenState Bool
    | OnBlur
    | OnKey Key
    | MenuItemSelected MenuMessage


type SkipAmount
    = First
    | Last
    | Exact Int


byId : Int -> Array DropdownItem -> DropdownItem
byId index items =
    case Array.get index items of
        Just t ->
            t

        Nothing ->
            emptyItem


update : Msg -> Dropdown -> ( Dropdown, Cmd msg )
update msg dropdown =
    case msg of
        ItemPicked item ->
            { dropdown | selectedItem = item, isOpen = False } ! []

        ItemEntered item ->
            { dropdown | highlightedItem = Just item } ! []

        ItemLeft item ->
            { dropdown | highlightedItem = Nothing } ! []

        SetOpenState newState ->
            { dropdown | isOpen = newState } ! []

        OnBlur ->
            { dropdown
                | isOpen = False
                , selectedItem = defaultSelectedItem dropdown.highlightedItem
            }
                ! []

        OnKey Esc ->
            { dropdown | isOpen = False } ! []

        OnKey Enter ->
            if dropdown.isOpen then
                { dropdown
                    | isOpen = False
                    , selectedItem = byId dropdown.highlightedIndex dropdown.dropdownSource
                }
                    ! []
            else
                dropdown ! []

        OnKey ArrowUp ->
            pickerSkip dropdown (Exact -1)

        OnKey ArrowDown ->
            pickerSkip dropdown (Exact 1)

        OnKey PageUp ->
            pickerSkip dropdown (Exact -9)

        OnKey PageDown ->
            pickerSkip dropdown (Exact 9)

        OnKey Home ->
            pickerSkip dropdown First

        OnKey End ->
            pickerSkip dropdown Last

        OnKey (Searchable char) ->
            updateSearchString char dropdown

        MenuItemSelected menuItem ->
            { dropdown | isOpen = not dropdown.isOpen } ! [ sendMenuMessageNew menuItem ]


boundedIndex : Array DropdownItem -> Int -> Int
boundedIndex dropdownSource index =
    if index < 0 then
        0
    else if index > Array.length dropdownSource then
        Array.length dropdownSource - 1
    else
        index


pickerSkip : Dropdown -> SkipAmount -> ( Dropdown, Cmd msg )
pickerSkip dropdown skipAmount =
    let
        newIndexCalc =
            case skipAmount of
                Exact skipCount ->
                    dropdown.highlightedIndex + skipCount

                First ->
                    0

                Last ->
                    Array.length dropdown.dropdownSource - 1

        newIndex =
            boundedIndex dropdown.dropdownSource newIndexCalc

        selectedItem =
            byId newIndex dropdown.dropdownSource
    in
        if dropdown.isOpen then
            { dropdown | highlightedIndex = newIndex } ! [ scrollToDomId (getId dropdown.id selectedItem) ]
        else
            { dropdown | highlightedIndex = newIndex, selectedItem = selectedItem } ! []


view : Dropdown -> Html Msg
view dropdown =
    let
        displayStyle =
            if dropdown.isOpen then
                ( "display", "block" )
            else
                ( "display", "none" )

        activeClass =
            if dropdown.isOpen then
                "e-focus e-popactive"
            else
                ""

        dropInputWidth =
            style [ ( "width", "100%" ) ]

        keyMsgDecoder =
            Events.keyCode
                |> Json.Decode.andThen (keyDecoder dropdown)
                |> Json.Decode.map OnKey
    in
        div [ Events.onWithOptions "keydown" { stopPropagation = True, preventDefault = True } keyMsgDecoder ]
            [ span
                [ onClick (SetOpenState (not dropdown.isOpen))
                , class ("e-ddl e-widget " ++ activeClass)
                , dropInputWidth
                ]
                [ span
                    [ class "e-in-wrap e-box" ]
                    [ input
                        [ class "e-input"
                        , readonly True
                        , value dropdown.selectedItem.name
                        , if dropdown.isOpen then
                            Events.onBlur OnBlur
                          else
                            style []
                        ]
                        []
                    , span [ class "e-select" ]
                        [ span [ class "e-icon e-arrow-sans-down" ] []
                        ]
                    ]
                ]
            , ul [ style <| displayStyle :: dropdownList, class "dropdown-ul" ] (viewItem dropdown)
            ]


updateGrid : List { b | isOpen : a, t : Bool } -> Msg -> { b | isOpen : a, t : c } -> ( List { b | isOpen : a, t : Bool }, Cmd msg )
updateGrid items msg dropdown =
    let
        newList newItem =
            items
                |> List.map
                    (\oldItem ->
                        if oldItem == newItem then
                            newItem
                        else
                            oldItem
                    )
    in
        case msg of
            ItemPicked item ->
                items ! []

            ItemEntered item ->
                -- { dropdown | highlightedItem = Just item } ! []
                items ! []

            ItemLeft item ->
                -- { dropdown | highlightedItem = Nothing } ! []
                items ! []

            SetOpenState newState ->
                let
                    t =
                        dropdown.isOpen
                in
                    newList { dropdown | t = newState } ! []

            -- { dropdown | isOpen = newState } ! []
            OnBlur ->
                items ! []

            -- { dropdown
            --     | isOpen = False
            --     , selectedItem = defaultSelectedItem dropdown.highlightedItem
            -- }
            --     ! []
            OnKey Esc ->
                -- { dropdown | isOpen = False } ! []
                items ! []

            OnKey Enter ->
                items ! []

            OnKey ArrowUp ->
                items ! []

            OnKey ArrowDown ->
                items ! []

            OnKey PageUp ->
                items ! []

            OnKey PageDown ->
                items ! []

            OnKey Home ->
                items ! []

            OnKey End ->
                items ! []

            OnKey (Searchable char) ->
                items ! []

            MenuItemSelected menuItem ->
                -- { dropdown | isOpen = not dropdown.isOpen } ! [ sendMenuMessageNew menuItem ]
                items ! []


viewGrid : Dropdown -> List ( String, String, MenuMessage ) -> Html Msg
viewGrid dropdown dropDownItems =
    let
        dropMenu =
            case dropdown.isOpen of
                True ->
                    [ ul [ class "e-menu e-js e-widget e-box e-separator" ]
                        (List.map dropDownMenuItem dropDownItems)
                    ]

                False ->
                    []

        btnClass =
            class "btn btn-sm btn-default fa fa-angle-down btn-context-menu editDropDown"

        btnStyle =
            style [ ( "position", "relative" ) ]
    in
        div
            [ style [ ( "text-align", "right" ) ]
            , onClick (SetOpenState (not dropdown.isOpen))
            ]
            [ button [ type_ "button", btnClass, btnStyle ]
                [ div [ dropDownMenuStyle ]
                    dropMenu
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


dropDownMenuItem : ( String, String, MenuMessage ) -> Html Msg
dropDownMenuItem ( iconClass, displayText, menuMessage ) =
    li [ class "e-content e-list" ]
        [ a [ class "e-menulink", Events.onClick (MenuItemSelected menuMessage), target "_blank" ]
            [ text displayText
            , span [ class ("e-gridcontext e-icon " ++ iconClass) ] []
            ]
        ]


getId : String -> DropdownItem -> String
getId id item =
    id ++ "-" ++ Functions.defaultIntToString item.id


viewItem : Dropdown -> List (Html Msg)
viewItem dropdown =
    let
        arraySize =
            Array.length dropdown.dropdownSource

        numItems =
            dropdown.dropdownSource
                |> Array.toList
                |> List.map (\t -> String.length t.name)
                |> List.sortBy identity
                |> List.reverse
                |> List.head
                |> Maybe.withDefault 150

        width =
            numItems * 6

        commonWidth =
            [ ( "width", toString width ++ "px" ) ]

        mouseActive =
            [ ( "background-color", "" ), ( "color", "#808080" ) ] ++ commonWidth

        keyActive =
            [ ( "background-color", "#f4f4f4" ), ( "color", "#333" ) ] ++ commonWidth
    in
        dropdown.dropdownSource
            |> Array.indexedMap
                (\index item ->
                    (li
                        [ onClick (ItemPicked item)
                        , Events.onMouseEnter (ItemEntered item)
                        , Events.onMouseLeave (ItemLeft item)
                        , class "dropdown-li"
                        , if dropdown.highlightedItem == Just item then
                            style mouseActive
                          else if dropdown.highlightedIndex == index && (index > 0 || dropdown.isOpen) then
                            style keyActive
                          else
                            style commonWidth
                        , Html.Attributes.id (getId dropdown.id item)
                        ]
                        [ text item.name ]
                    )
                )
            |> Array.toList


onClick : msg -> Attribute msg
onClick message =
    Events.onWithOptions "click"
        { stopPropagation = True, preventDefault = False }
        (Json.Decode.succeed message)



-- styles for list container


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


maybeFallback : Maybe a -> Maybe a -> Maybe a
maybeFallback replacement original =
    case original of
        Just _ ->
            original

        Nothing ->
            replacement


updateSearchString : Char -> Dropdown -> ( Dropdown, Cmd msg )
updateSearchString searchChar dropdown =
    let
        searchString =
            -- Manage backspace character
            if searchChar == '\x08' then
                String.dropRight 1 dropdown.searchString
            else
                dropdown.searchString ++ String.toLower (String.fromChar searchChar)

        maybeSelectedItem =
            dropdown.dropdownSource
                |> Array.toList
                |> List.filter (\t -> String.startsWith searchString (String.toLower t.name))
                |> List.head
    in
        case maybeSelectedItem of
            Just t ->
                { dropdown
                    | selectedItem = t
                    , searchString = searchString
                }
                    ! [ scrollToDomId (getId dropdown.id t) ]

            Nothing ->
                dropdown ! [ Cmd.none ]


keyDecoder : Dropdown -> Int -> Json.Decode.Decoder Key
keyDecoder dropdown keyCode =
    let
        -- This is necessary to ensure that the key is not consumed and can propagate to the parent
        pass =
            Json.Decode.fail ""

        key =
            Json.Decode.succeed
    in
        case keyCode of
            13 ->
                key Enter

            27 ->
                -- Consume Esc only if the Menu is open
                if dropdown.isOpen then
                    pass
                else
                    key Esc

            -- 32 ->
            --     key Space
            33 ->
                key PageUp

            34 ->
                key PageDown

            35 ->
                key End

            36 ->
                key Home

            38 ->
                key ArrowUp

            40 ->
                key ArrowDown

            _ ->
                let
                    char =
                        Char.fromCode keyCode

                    -- TODO should the user be able to search non-alphanum chars?
                    -- TODO add support for non-ascii alphas
                    isAlpha char =
                        (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z')
                in
                    -- Backspace is "searchable" because it can be used to modify the search string
                    if isAlpha char || Char.isDigit char || char == '\x08' then
                        key (Searchable char)
                    else
                        pass
