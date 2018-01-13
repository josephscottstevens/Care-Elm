port module Utils.Dropdown exposing (Dropdown, Msg, init, update, view)

import Html exposing (Html, Attribute, div, span, text, li, ul, input)
import Html.Attributes exposing (style, value, class, readonly)
import Html.Events as Events
import Json.Decode
import Utils.CommonTypes exposing (DropdownItem)
import Utils.CommonFunctions as Functions
import Char


port dropdownMenuScroll : String -> Cmd msg


scrollToDomId : String -> Cmd msg
scrollToDomId =
    dropdownMenuScroll


type alias Dropdown =
    { isOpen : Bool
    , selectedId : Maybe Int
    , mouseSelectedId : Maybe Int
    , keyboardSelectedId : Maybe Int
    , searchString : String
    , domId : String
    }


init : String -> Maybe Int -> Dropdown
init domId selectedId =
    { isOpen = False
    , selectedId = selectedId
    , mouseSelectedId = Nothing
    , keyboardSelectedId = Nothing
    , searchString = ""
    , domId = domId
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
    | ItemLeft
    | SetOpenState Bool
    | OnBlur
    | OnKey Key


type SkipAmount
    = First
    | Last
    | Exact Int


byId : Int -> List DropdownItem -> DropdownItem
byId id items =
    items
        |> List.filter (\t -> t.id == Just id)
        |> List.head
        |> Maybe.withDefault (DropdownItem Nothing "")


getDropdownText : Maybe Int -> List DropdownItem -> String
getDropdownText id dropdownItems =
    dropdownItems
        |> List.filter (\t -> t.id == id)
        |> List.map (\t -> t.name)
        |> List.head
        |> Maybe.withDefault ""


update : Msg -> Dropdown -> List DropdownItem -> ( Dropdown, Cmd msg )
update msg dropdown dropdownItems =
    case msg of
        ItemPicked item ->
            { dropdown | selectedId = item.id, isOpen = False } ! []

        ItemEntered item ->
            { dropdown | mouseSelectedId = item.id } ! []

        ItemLeft ->
            { dropdown | mouseSelectedId = Nothing } ! []

        SetOpenState newState ->
            { dropdown | isOpen = newState } ! []

        OnBlur ->
            { dropdown
                | isOpen = False
                , selectedId = dropdown.mouseSelectedId
            }
                ! []

        OnKey Esc ->
            { dropdown | isOpen = False } ! []

        OnKey Enter ->
            if dropdown.isOpen then
                { dropdown
                    | isOpen = False
                    , selectedId = dropdown.mouseSelectedId
                }
                    ! []
            else
                dropdown ! []

        OnKey ArrowUp ->
            pickerSkip dropdown (Exact -1) dropdownItems

        OnKey ArrowDown ->
            pickerSkip dropdown (Exact 1) dropdownItems

        OnKey PageUp ->
            pickerSkip dropdown (Exact -9) dropdownItems

        OnKey PageDown ->
            pickerSkip dropdown (Exact 9) dropdownItems

        OnKey Home ->
            pickerSkip dropdown First dropdownItems

        OnKey End ->
            pickerSkip dropdown Last dropdownItems

        OnKey (Searchable char) ->
            updateSearchString char dropdown dropdownItems


boundedIndex : List DropdownItem -> Int -> Int
boundedIndex dropdownSource index =
    if index < 0 then
        0
    else if index > List.length dropdownSource then
        List.length dropdownSource - 1
    else
        index


pickerSkip : Dropdown -> SkipAmount -> List DropdownItem -> ( Dropdown, Cmd msg )
pickerSkip dropdown skipAmount dropdownItems =
    let
        newIndexCalc =
            case skipAmount of
                Exact skipCount ->
                    (Functions.defaultInt dropdown.keyboardSelectedId) + skipCount

                First ->
                    0

                Last ->
                    List.length dropdownItems - 1

        newIndex =
            boundedIndex dropdownItems newIndexCalc

        selectedItem =
            byId newIndex dropdownItems
    in
        if dropdown.isOpen then
            { dropdown | keyboardSelectedId = Just newIndex } ! [ scrollToDomId dropdown.domId ]
        else
            { dropdown | keyboardSelectedId = Just newIndex, selectedId = selectedItem.id } ! []


view : Dropdown -> List DropdownItem -> Msg -> Html msg
view dropdown dropdownItems event =
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
                        , value (getDropdownText dropdown.selectedId dropdownItems)
                        , if dropdown.isOpen then
                            Events.onBlur event.OnBlur
                          else
                            style []
                        ]
                        []
                    , span [ class "e-select" ]
                        [ span [ class "e-icon e-arrow-sans-down" ] []
                        ]
                    ]
                ]
            , ul [ style <| displayStyle :: dropdownList, class "dropdown-ul" ] (viewItem dropdown dropdownItems)
            ]


getId : String -> DropdownItem -> String
getId id item =
    id ++ "-" ++ Functions.defaultIntToString item.id


viewItem : Dropdown -> List DropdownItem -> List (Html Msg)
viewItem dropdown dropdownItems =
    let
        numItems =
            dropdownItems
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
        dropdownItems
            |> List.indexedMap
                (\index item ->
                    li
                        [ onClick (ItemPicked item)
                        , Events.onMouseEnter (ItemEntered item)
                        , Events.onMouseLeave ItemLeft
                        , class "dropdown-li"
                        , if dropdown.mouseSelectedId == item.id then
                            style mouseActive
                          else if dropdown.keyboardSelectedId == Just index && (index > 0 || dropdown.isOpen) then
                            style keyActive
                          else
                            style commonWidth
                        , Html.Attributes.id (getId dropdown.domId item)
                        ]
                        [ text item.name ]
                )


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


updateSearchString : Char -> Dropdown -> List DropdownItem -> ( Dropdown, Cmd msg )
updateSearchString searchChar dropdown dropdownItems =
    let
        searchString =
            -- Manage backspace character
            if searchChar == '\x08' then
                String.dropRight 1 dropdown.searchString
            else
                dropdown.searchString ++ String.toLower (String.fromChar searchChar)

        maybeSelectedItem =
            dropdownItems
                |> List.filter (\t -> String.startsWith searchString (String.toLower t.name))
                |> List.head
    in
        case maybeSelectedItem of
            Just t ->
                { dropdown
                    | selectedId = t.id
                    , searchString = searchString
                }
                    ! [ scrollToDomId (getId dropdown.domId t) ]

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

                    isAlpha char =
                        (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z')
                in
                    -- Backspace is "searchable" because it can be used to modify the search string
                    if isAlpha char || Char.isDigit char || char == '\x08' then
                        key (Searchable char)
                    else
                        pass
