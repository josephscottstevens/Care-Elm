port module Common.Dropdown exposing (DropState, Msg, init, update, view)

import Html exposing (Html, Attribute, div, span, text, li, ul, input)
import Html.Attributes exposing (style, value, class, readonly, placeholder, tabindex)
import Html.Events as Events
import Json.Decode
import Common.Types exposing (DropdownItem)
import Common.Functions as Functions
import Char


port dropdownMenuScroll : String -> Cmd msg


scrollToDomId : String -> Maybe Int -> Cmd msg
scrollToDomId str id =
    let
        newId =
            Maybe.withDefault 1 id
    in
        dropdownMenuScroll (str ++ "-" ++ toString newId)


type alias DropState =
    { isOpen : Bool
    , mouseSelectedId : Maybe (Maybe Int)
    , keyboardSelectedId : Maybe Int
    , searchString : String
    , domId : String
    }


init : String -> DropState
init domId =
    { isOpen = False
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


type SkipAmount
    = First
    | Last
    | Exact Int


type Msg
    = ItemClicked DropdownItem
    | ItemEntered DropdownItem
    | ItemLeft
    | SetOpenState Bool
    | OnBlur
    | OnKey Key


update : Msg -> DropState -> Maybe Int -> List DropdownItem -> ( DropState, Maybe Int, Cmd msg )
update msg dropdown selectedId dropdownItems =
    case msg of
        ItemClicked item ->
            ( { dropdown | isOpen = False }, item.id, Cmd.none )

        ItemEntered item ->
            ( { dropdown | mouseSelectedId = Just item.id }, selectedId, Cmd.none )

        ItemLeft ->
            ( { dropdown | mouseSelectedId = Nothing }, selectedId, Cmd.none )

        SetOpenState newState ->
            ( { dropdown | isOpen = newState, keyboardSelectedId = selectedId }, selectedId, scrollToDomId dropdown.domId selectedId )

        OnBlur ->
            case dropdown.mouseSelectedId of
                Just mouseSelectedId ->
                    ( { dropdown | isOpen = False }, mouseSelectedId, Cmd.none )

                Nothing ->
                    ( { dropdown | isOpen = False }, selectedId, Cmd.none )

        OnKey Esc ->
            ( { dropdown | isOpen = False }, selectedId, Cmd.none )

        OnKey Enter ->
            if dropdown.isOpen then
                ( { dropdown | isOpen = False }, dropdown.keyboardSelectedId, Cmd.none )
            else
                ( dropdown, selectedId, Cmd.none )

        OnKey ArrowUp ->
            pickerSkip dropdown (Exact -1) dropdownItems selectedId

        OnKey ArrowDown ->
            pickerSkip dropdown (Exact 1) dropdownItems selectedId

        OnKey PageUp ->
            pickerSkip dropdown (Exact -9) dropdownItems selectedId

        OnKey PageDown ->
            pickerSkip dropdown (Exact 9) dropdownItems selectedId

        OnKey Home ->
            pickerSkip dropdown First dropdownItems selectedId

        OnKey End ->
            pickerSkip dropdown Last dropdownItems selectedId

        OnKey (Searchable char) ->
            updateSearchString char dropdown dropdownItems selectedId


pickerSkip : DropState -> SkipAmount -> List DropdownItem -> Maybe Int -> ( DropState, Maybe Int, Cmd msg )
pickerSkip dropdown skipAmount dropdownItems selectedId =
    let
        newIndexCalc =
            case skipAmount of
                Exact skipCount ->
                    Functions.defaultInt dropdown.keyboardSelectedId + skipCount

                First ->
                    0

                Last ->
                    List.length dropdownItems - 1

        newIndex =
            if newIndexCalc < 0 then
                0
            else if newIndexCalc > List.length dropdownItems then
                List.length dropdownItems - 1
            else
                newIndexCalc

        selectedItem =
            dropdownItems
                |> List.filter (\t -> t.id == Just newIndex)
                |> List.head
                |> Maybe.withDefault (DropdownItem Nothing "")
    in
        if dropdown.isOpen then
            ( { dropdown | keyboardSelectedId = Just newIndex }, selectedId, scrollToDomId dropdown.domId selectedItem.id )
        else
            ( { dropdown | keyboardSelectedId = Just newIndex }, Just newIndex, Cmd.none )


view : DropState -> List DropdownItem -> Maybe Int -> Html Msg
view dropdown dropdownItems selectedId =
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

        getDropdownText =
            dropdownItems
                |> List.filter (\t -> t.id == selectedId)
                |> List.map .name
                |> List.head
                |> Maybe.withDefault ""
    in
        div
            [ Events.onWithOptions "keydown" { stopPropagation = True, preventDefault = True } keyMsgDecoder
            , Events.onBlur OnBlur
            , Events.onClick (SetOpenState (not dropdown.isOpen))
            , tabindex 0
            ]
            [ span
                [ class ("e-ddl e-widget " ++ activeClass)
                , dropInputWidth
                ]
                [ span
                    [ class "e-in-wrap e-box" ]
                    [ input
                        [ class "noselect e-input"
                        , readonly True
                        , value getDropdownText
                        , tabindex -1
                        , placeholder "Choose..."
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


viewItem : DropState -> List DropdownItem -> List (Html Msg)
viewItem dropdown dropdownItems =
    let
        biggestStrLength =
            dropdownItems
                |> List.map (\t -> String.length t.name)
                |> List.sortBy identity
                |> List.reverse
                |> List.head
                |> Maybe.withDefault 150

        commonWidth =
            [ ( "width", toString biggestStrLength ++ "px" )
            , ( "min-width", "99.7%" )
            ]

        mouseActive =
            [ ( "background-color", "" ), ( "color", "#808080" ) ] ++ commonWidth

        keyActive =
            [ ( "background-color", "#f4f4f4" ), ( "color", "#333" ) ] ++ commonWidth
    in
        dropdownItems
            |> List.map
                (\item ->
                    li
                        [ Events.onMouseEnter (ItemEntered item)
                        , Events.onMouseLeave ItemLeft
                        , Events.onClick (ItemClicked item)
                        , class "noselect dropdown-li"
                        , if dropdown.mouseSelectedId == Just item.id then
                            style mouseActive
                          else if dropdown.keyboardSelectedId == item.id && dropdown.isOpen then
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
    , ( "background-color", "white" )
    , ( "max-height", "152px" )
    , ( "overflow-x", "hidden" )
    , ( "overflow-y", "scroll" )
    , ( "z-index", "100" )
    , ( "min-width", "74%" )
    ]


updateSearchString : Char -> DropState -> List DropdownItem -> Maybe Int -> ( DropState, Maybe Int, Cmd msg )
updateSearchString searchChar dropdown dropdownItems selectedId =
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
                --todo... why was this here? , selectedId = selectedItem.id
                ( { dropdown | searchString = searchString }, selectedId, Cmd.none )

            --TODO
            --! [ scrollToDomId (getId dropdown.domId t.id) ]
            Nothing ->
                ( dropdown, selectedId, Cmd.none )


keyDecoder : DropState -> Int -> Json.Decode.Decoder Key
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
