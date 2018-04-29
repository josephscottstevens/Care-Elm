port module Common.Dropdown exposing (DropState, init, view, getDropdownText)

import Html exposing (Html, div, span, text, li, ul, input)
import Html.Attributes exposing (style, value, class, readonly, placeholder, tabindex, disabled)
import Html.Events as Events
import Json.Decode
import Common.Types exposing (DropdownItem)
import Common.Functions as Functions
import Char


port dropdownMenuScroll : String -> Cmd msg


scrollToDomId : String -> Maybe Int -> Cmd msg
scrollToDomId str id =
    dropdownMenuScroll (str ++ "-" ++ Functions.defaultIntToString id)


type alias DropState =
    { isOpen : Bool
    , keyboardSelectedIndex : Int
    , searchString : String
    , domId : String
    , showSearchText : Bool
    }


init : String -> Bool -> DropState
init domId showSearchText =
    { isOpen = False
    , keyboardSelectedIndex = 0
    , searchString = ""
    , domId = domId
    , showSearchText = showSearchText
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


getIndex : Maybe Int -> List DropdownItem -> Int
getIndex selectedId dropdownItems =
    dropdownItems
        |> List.indexedMap
            (\idx t ->
                if t.id == selectedId then
                    Just idx
                else
                    Nothing
            )
        |> List.filterMap identity
        |> List.head
        |> Maybe.withDefault 0


getId : List DropdownItem -> Int -> Maybe Int
getId dropdownItems index =
    dropdownItems
        |> Functions.getAt index
        |> Maybe.map .id
        |> Maybe.withDefault Nothing


onBlur : DropState -> Maybe Int -> ( DropState, Maybe Int, Cmd msg )
onBlur dropState selectedId =
    ( { dropState | isOpen = False, searchString = "" }, selectedId, Cmd.none )


onItemClicked : DropState -> DropdownItem -> ( DropState, Maybe Int, Cmd msg )
onItemClicked dropdown dropdownItem =
    ( { dropdown | isOpen = False, searchString = "" }, dropdownItem.id, Cmd.none )


onOpen : DropState -> Maybe Int -> List DropdownItem -> Bool -> ( DropState, Maybe Int, Cmd msg )
onOpen dropdown selectedId dropdownItems newState =
    ( { dropdown
        | isOpen = newState
        , keyboardSelectedIndex = getIndex selectedId dropdownItems
      }
    , selectedId
    , scrollToDomId dropdown.domId selectedId
    )


onKey : DropState -> Maybe Int -> List DropdownItem -> Key -> ( DropState, Maybe Int, Cmd msg )
onKey dropdown selectedId dropdownItems key =
    case key of
        Esc ->
            ( { dropdown | isOpen = False, searchString = "" }, selectedId, Cmd.none )

        Enter ->
            let
                newDropdown =
                    { dropdown | isOpen = False, searchString = "" }
            in
                if dropdown.isOpen then
                    ( newDropdown, getId dropdownItems dropdown.keyboardSelectedIndex, Cmd.none )
                else
                    ( newDropdown, selectedId, Cmd.none )

        ArrowUp ->
            pickerSkip dropdown (Exact -1) dropdownItems selectedId

        ArrowDown ->
            pickerSkip dropdown (Exact 1) dropdownItems selectedId

        PageUp ->
            pickerSkip dropdown (Exact -9) dropdownItems selectedId

        PageDown ->
            pickerSkip dropdown (Exact 9) dropdownItems selectedId

        Home ->
            pickerSkip dropdown First dropdownItems selectedId

        End ->
            pickerSkip dropdown Last dropdownItems selectedId

        Searchable char ->
            updateSearchString char dropdown dropdownItems selectedId


resetSearchString : DropState -> Maybe Int -> ( DropState, Maybe Int, Cmd msg )
resetSearchString dropState selectedId =
    ( { dropState | searchString = "" }, selectedId, Cmd.none )


pickerSkip : DropState -> SkipAmount -> List DropdownItem -> Maybe Int -> ( DropState, Maybe Int, Cmd msg )
pickerSkip dropdown skipAmount dropdownItems selectedId =
    let
        newIndexCalc =
            case skipAmount of
                Exact skipCount ->
                    dropdown.keyboardSelectedIndex + skipCount

                First ->
                    0

                Last ->
                    List.length dropdownItems - 1

        newIndex =
            if newIndexCalc < 0 then
                0
            else if newIndexCalc >= List.length dropdownItems then
                List.length dropdownItems - 1
            else
                newIndexCalc

        newSelectedId =
            getId dropdownItems newIndex

        newDropdown =
            { dropdown
                | keyboardSelectedIndex = newIndex
                , searchString = ""
            }
    in
        if dropdown.isOpen then
            ( newDropdown, selectedId, scrollToDomId dropdown.domId newSelectedId )
        else
            ( newDropdown, Just newIndex, Cmd.none )


getDropdownText : List DropdownItem -> Maybe Int -> String
getDropdownText dropdownItems selectedId =
    dropdownItems
        |> List.filter (\t -> t.id == selectedId)
        |> List.map .name
        |> List.head
        |> Maybe.withDefault ""


commonWidth : ( String, String )
commonWidth =
    ( "min-width", "99.7%" )


view : DropState -> (( DropState, Maybe Int, Cmd msg ) -> msg) -> List DropdownItem -> Maybe Int -> Html msg
view dropdown toMsg dropdownItems selectedId =
    let
        activeClass =
            if dropdown.isOpen then
                "e-focus e-popactive"
            else
                ""

        getDropdownText =
            dropdownItems
                |> List.filter (\t -> t.id == selectedId)
                |> List.map .name
                |> List.head
                |> Maybe.withDefault ""

        dropdownWidthMultiplier =
            if dropdown.showSearchText then
                9
            else
                7

        keyMsgDecoder =
            Events.keyCode
                |> Json.Decode.andThen (keyDecoder dropdown)
                |> Json.Decode.map (\t -> toMsg (onKey dropdown selectedId dropdownItems t))

        biggestStrLength =
            dropdownItems
                |> List.map (\t -> String.length t.name * dropdownWidthMultiplier)
                |> List.sortBy identity
                |> List.reverse
                |> List.head
                |> Maybe.withDefault 150
    in
        div
            [ Events.onWithOptions "keydown" { stopPropagation = True, preventDefault = True } keyMsgDecoder
            , if dropdown.isOpen then
                Events.onBlur (toMsg (onBlur dropdown selectedId))
              else
                disabled False
            , if dropdown.isOpen then
                disabled False
              else
                Events.onClick (toMsg <| onOpen dropdown selectedId dropdownItems True)
            , tabindex 0

            -- Make div focusable, since we need the on blur to trigger for both child elements
            , style [ ( "position", "relative" ), ( "width", "100%" ) ]
            , class "dropdown-outline"
            ]
            [ span
                [ class ("e-ddl e-widget " ++ activeClass)
                , style [ ( "width", "100%" ) ]
                ]
                [ span
                    [ class "e-in-wrap e-box"
                    ]
                    [ input
                        [ class "noselect e-input"
                        , readonly True
                        , value getDropdownText
                        , tabindex -1 -- Make it so you cannot set focus via tabbing, we need root div to have the focus
                        , disabled True -- Make it so you cannot click to set focus, we need root div to have the focus
                        , placeholder "Choose..."
                        ]
                        []
                    , span [ class "e-select" ]
                        [ span [ class "e-icon e-arrow-sans-down" ] []
                        ]
                    ]
                ]
            , div
                [ style
                    [ if dropdown.isOpen then
                        ( "display", "block" )
                      else
                        ( "display", "none" )
                    , ( "width", toString biggestStrLength ++ "px" )
                    , ( "position", "absolute" )
                    , ( "top", "32px" )
                    , ( "height", "42px" )
                    , ( "border-top-left-radius", "4px" )
                    , ( "border-top-right-radius", "4px" )
                    , ( "border-color", "#cecece" )
                    , ( "border-style", "solid" )
                    , ( "border-width", "1px 1px 0 1px" )
                    , ( "box-shadow", "0 1px 2px rgba(0,0,0,.24)" )
                    , ( "padding", "0" )
                    , ( "margin", "0" )
                    , ( "background-color", "white" )
                    , ( "max-height", "152px" )
                    , ( "overflow-x", "hidden" )
                    , ( "overflow-y", "hidden" )
                    , ( "z-index", "100" )
                    , ( "min-width", "99.7%" )
                    ]
                ]
                [ if dropdown.showSearchText then
                    span
                        [ class "e-atc e-search"
                        , style
                            [ ( "min-width", "96%" )
                            , ( "margin-left", "8px" )
                            , ( "margin-right", "6px" )
                            , ( "margin-top", "6px" )
                            ]
                        ]
                        [ span [ class "e-in-wrap" ]
                            [ input
                                [ class "noselect e-input"
                                , value dropdown.searchString
                                ]
                                []
                            , span [ class "e-icon e-search", style [ ( "width", "14px" ), ( "right", "10px" ), ( "color", "#cecece" ), ( "position", "absolute" ) ] ] []
                            ]
                        ]
                  else
                    text ""
                ]
            , div
                [ style
                    [ if dropdown.isOpen then
                        ( "display", "block" )
                      else
                        ( "display", "none" )
                    , ( "width", toString biggestStrLength ++ "px" )
                    , ( "position", "absolute" )
                    , if dropdown.showSearchText then
                        ( "top", "74px" )
                      else
                        ( "top", "32px" )
                    , ( "border-radius", "4px" )
                    , ( "border-top-left-radius", "0" )
                    , ( "border-top-right-radius", "0" )
                    , ( "box-shadow", "0 1px 2px rgba(0,0,0,.24)" )
                    , ( "padding", "0" )
                    , ( "margin", "0" )
                    , ( "background-color", "white" )
                    , ( "max-height", "152px" )
                    , ( "overflow-x", "hidden" )
                    , ( "overflow-y", "scroll" )
                    , ( "z-index", "100" )
                    , ( "min-width", "99.7%" )
                    ]
                , class "dropdown-ul"
                ]
                [ ul
                    [ style
                        [ ( "padding-left", "0" )
                        , ( "padding-right", "1px" )
                        , ( "-webkit-margin-after", "0" )
                        ]
                    ]
                    (viewItem dropdown toMsg dropdownItems)
                ]
            ]


viewItem : DropState -> (( DropState, Maybe Int, Cmd msg ) -> msg) -> List DropdownItem -> List (Html msg)
viewItem dropdown toMsg dropdownItems =
    let
        keyActive =
            [ ( "background-color", "#f4f4f4" ), ( "color", "#333" ) ] ++ [ commonWidth ]
    in
        (dropdownItems
            |> List.map
                (\item ->
                    li
                        [ Events.onClick (toMsg (onItemClicked dropdown item))
                        , class "noselect dropdown-li"
                        , if dropdown.keyboardSelectedIndex == getIndex item.id dropdownItems && dropdown.isOpen then
                            style keyActive
                          else
                            style [ commonWidth ]
                        , Html.Attributes.id (dropdown.domId ++ "-" ++ Functions.defaultIntToString item.id)
                        ]
                        [ text item.name ]
                )
        )


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
                ( { dropdown
                    | searchString = searchString
                    , keyboardSelectedIndex = getIndex t.id dropdownItems
                  }
                , selectedId
                , scrollToDomId dropdown.domId t.id
                )

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
